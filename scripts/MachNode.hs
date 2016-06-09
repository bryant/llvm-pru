{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module MachNode where

import qualified SDNode
import qualified Control.Monad.Trans.State as State
import Control.Monad.Trans.State (State)
import SDNode (SDNode(SDPatLeaf), TypeShow, SDKind, ShowWrap(E_))
import Data.Proxy (Proxy(Proxy))
import GHC.TypeLits (Nat, KnownNat, natVal)

data IAttr
    = IsReturn Bool | IsBranch Bool | IsTerminator Bool | HasSideEffects Bool
    | IsPseudo Bool | IsMoveImm Bool | Uses [String] | Defs [String]
    | UsesCustomInserter Bool | IsBarrier Bool | IsRematerializable Bool

-- kind of machine nodes
data MachKind where
    L :: MachKind
    N :: MachKind
    I :: ins -> MachKind

type family a :->> b where
    '[] :->> b = b
    (a ': as) :->> b = a -> as :->> b

type family TypeMap tf ts where
    TypeMap tf '[] = '[]
    TypeMap tf (t ': ts) = tf t ': TypeMap tf ts

type family Append u v where
    Append '[] vs = vs
    Append (u ': us) vs = u ': Append us vs

data MachNode (to_leaf :: MachKind) (t :: SDKind) where
    EXTRACT_SUBREG :: MachNode to_leaf t -> String -> MachNode N s
    COPY_TO_REGCLASS :: MachNode to_leaf t -> MachNode N s
    SUBREG_TO_REG :: MachNode to_leaf t -> String -> MachNode N s
    MachInstr :: C ins a => [IAttr]
              -> String  -- llvm name
              -> (TypeMap (MachNode L) (Append ins '[a]) :->> String)
              -- ^ asm printer
              -> [MachWrap]  -- typed list of operands
              -> MachNode (I ins) a
    MachLeaf :: TypeShow a => SDNode SDNode.L a -> MachNode L a

data MachWrap = forall l a. M_ (MachNode l a)

instance Show MachWrap where
    show (M_ inner) = show inner

-- maps an SDPatLeaf to MachLeaf.
m :: TypeShow a => SDNode SDNode.L a -> MachNode L a
m = MachLeaf

instance Show (MachNode l t) where
    show (EXTRACT_SUBREG src subreg) =
        SDNode.op_print "EXTRACT_SUBREG" [E_ src, E_ subreg]
    show (COPY_TO_REGCLASS src) = SDNode.op_print "COPY_TO_REGCLASS" [E_ src]
    show (SUBREG_TO_REG src subreg) =
        SDNode.op_print "SUBREG_TO_REG" [E_ src, E_ subreg]
    show (MachInstr _ opname _ xs) = SDNode.op_print opname xs
    show (MachLeaf (SDPatLeaf "" n)) =
        SDNode.mach_type_show (Proxy :: Proxy t) ++ ": $op" ++ show n
    show (MachLeaf p) = show p

class C t u where
    do2 :: Proxy t -> Proxy u
        -> MachGen (TypeMap (MachNode L) (Append t '[u]) :->> b)
        -> MachGen b
    getops :: Proxy t -> Proxy u -> MachGen ([String], Maybe String)

instance {-# OVERLAPPING #-} C '[] SDNode.SDUnit where
    do2 _ _ f = f <*> pure undefined
    getops _ _ = pure ([], Nothing)

instance {-# OVERLAPPABLE #-} TypeShow u => C '[] u where
    do2 _ _ f = f <*> new_mach_op
    getops _ _ = do
        s <- new_mach_op :: MachGen (MachNode L u)
        return ([], Just $ show s)

instance {-# OVERLAPPING #-} (TypeShow t, C ts SDNode.SDUnit) => C (t ': ts) SDNode.SDUnit where
    do2 _ _ f = do2 (Proxy :: Proxy ts) (Proxy :: Proxy SDNode.SDUnit) (f <*> new_mach_op)
    getops _ _ = do
        s <- new_mach_op :: MachGen (MachNode L t)
        (rest, destop) <- getops (Proxy :: Proxy ts) (Proxy :: Proxy SDNode.SDUnit)
        return (show s : rest, destop)

instance {-# OVERLAPPABLE #-} (TypeShow t, TypeShow u, C ts u) => C (t ': ts) u where
    do2 _ _ f = do2 (Proxy :: Proxy ts) (Proxy :: Proxy u) (f <*> new_mach_op)
    getops _ _ = do
        s <- new_mach_op :: MachGen (MachNode L t)
        (rest, destop) <- getops (Proxy :: Proxy ts) (Proxy :: Proxy u)
        return (show s : rest, destop)

type MachGen = State Int

wrapped_sd_op :: TypeShow a => String -> MachGen (SDNode SDNode.L a)
wrapped_sd_op wrapper = SDPatLeaf wrapper <$> next_int
    where next_int = State.get >>= \n -> State.modify (+ 1) >> return n

new_sd_op :: TypeShow a => MachGen (SDNode SDNode.L a)
new_sd_op = wrapped_sd_op ""

new_mach_op :: TypeShow a => MachGen (MachNode L a)
new_mach_op = MachLeaf <$> new_sd_op

get_operands :: forall ins dty. C ins dty => MachNode (I ins) dty
             -> ([String], Maybe String)
get_operands _ =  fst $ State.runState p 0
    where p = getops (Proxy :: Proxy ins) (Proxy :: Proxy dty)

get_asm_string :: forall ins dty. C ins dty => MachNode (I ins) dty -> String
get_asm_string (MachInstr _ _ asmpr _) = fst $ State.runState p 0
    where p = do2 (Proxy :: Proxy ins) (Proxy :: Proxy dty) (pure asmpr)

{-
class FillInstrOperands t where
    fill_operands :: t -> MachGen (MachNode MachInstr)

instance {-# OVERLAPPING #-} FillInstrOperands (MachNode MachInstr) where
    fill_operands = pure

instance {-# OVERLAPPABLE #-} (TypeShow k, FillInstrOperands t) =>
        FillInstrOperands (SDNode L k -> t) where
    fill_operands f = fill_operands =<< fmap f new_sd_op

print_instr instr = node_shit . fst $ State.runState (fill_operands instr) 0

-- print_instr technique: a -> b

-- this is needed for nested MachInstrs
class Fancy t where i :: t a -> MachNode a
instance MachNodeable MachNode where to_mach_node = id
instance MachNodeable (SDNode L) where to_mach_node = MachLeaf
-}

extract_subreg :: TypeShow a => MachNode n a -> String -> MachNode N s
extract_subreg = EXTRACT_SUBREG

subreg_to_reg :: TypeShow a => MachNode l a -> String -> MachNode N s
subreg_to_reg = SUBREG_TO_REG

copy_to_regclass :: TypeShow a => MachNode l a -> MachNode N s
copy_to_regclass = COPY_TO_REGCLASS
