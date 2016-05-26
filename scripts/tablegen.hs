{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Control.Monad.Trans.State as State
import qualified Data.List as List
import Control.Monad.Trans.State (State)
import GHC.TypeLits (Symbol, Nat, CmpNat, KnownNat, natVal)
import Data.Proxy (Proxy(Proxy))

data PatternArg a = PatternArg Int

data MachineNode
    = Instruction { llvm_name :: String
                  , asm_format :: String
                  , attributes :: [IAttr]
                  }
    | EXTRACT_SUBREG | SUBREG_TO_REG | COPY_TO_REGCLASS

data IAttr
    = IsReturn Bool | IsBranch Bool | IsTerminator Bool | HasSideEffects Bool
    | IsPseudo Bool | IsMoveImm Bool | Uses [String] | Defs [String]
    | UsesCustomInserter Bool | IsBarrier Bool | IsRematerializable Bool

-- kind that denotes SDNode leaf-ness
data LeafKind = L | N

data SDNode (l :: LeafKind) (a :: SDKind) where
    SDUnOp :: SDIsInt a => String -> SDNode l a -> SDNode N a
    SDBinOp :: SDIsInt a => String -> SDNode k a -> SDNode l a -> SDNode N a
    SDShiftOp :: (SDIsInt a, SDIsInt b) => String -> SDNode k a -> SDNode l b
              -> SDNode N a
    SDTruncOp :: SDGT a b => SDNode l a -> SDNode N b
    SDExtOp :: SDGT b a => String -> SDNode l a -> SDNode N b
    SDBuildPair :: SDGT b a => SDNode k a -> SDNode l a -> SDNode N b
    SDLoadOp :: SDIsInt a => SDNode l SDPtr -> SDNode N a
    SDStoreOp :: SDIsInt a => SDNode k SDPtr -> SDNode l a -> SDNode N SDUnit
    SDPatLeaf :: TypeShow a => String -> Int -> SDNode L a

-- Operand kind
data SDKind = SDI Nat | Imm Nat | SDPtr | SDUnit | BasicBlock

type I32 = SDI 32
type I16 = SDI 16
type I8 = SDI 8

class SDIsInt z

instance SDIsInt (SDI n)
instance SDIsInt (Imm n)

type family SDKindWidth a where
    SDKindWidth (SDI n) = n
    SDKindWidth (Imm n) = n

type family SDGT a b where
    SDGT m n = CmpNat (SDKindWidth m) (SDKindWidth n) ~ GT

data ExistSDNode = forall l a. E_ (SDNode l a)

-- TODO: `show` expects a haskell-readable output. consider switching to a
-- different typeclass.
instance Show ExistSDNode where
    show (E_ node@SDPatLeaf{}) = show node
    show (E_ node) = parens $ show node

instance Show (SDNode l a) where
    show (SDUnOp opname n) = op_print opname [E_ n]
    show (SDBinOp opname l r) = op_print opname [E_ l, E_ r]
    show (SDShiftOp opname src sh) = op_print opname [E_ src, E_ sh]
    show (SDTruncOp n) = op_print "trunc" [E_ n]
    show (SDExtOp extttype n) = op_print extttype [E_ n]
    show (SDBuildPair l r) = op_print "build_pair" [E_ l, E_ r]
    show (SDLoadOp p) = op_print "load" [E_ p]
    show (SDStoreOp p val) = op_print "store" [E_ p, E_ val]
    show (SDPatLeaf wrapper leafid) = prefix ++ ": $op" ++ show leafid
        where
        prefix
            | wrapper /= "" = wrapper
            | otherwise = type_show (Proxy :: Proxy a)

class TypeShow t where type_show :: Proxy t -> String

instance KnownNat n => TypeShow (SDI n) where
    type_show _ = "i" ++ show (natVal (Proxy :: Proxy n))

instance KnownNat n => TypeShow (Imm n) where
    type_show _ = "i" ++ show (natVal (Proxy :: Proxy n)) ++ "imm"

op_print :: String -> [ExistSDNode] -> String
op_print opname [] = opname
op_print opname xs = opname ++ " " ++ comma_join (map show xs)

comma_join :: [String] -> String
comma_join = List.intercalate ", "

parens xs = "(" ++ xs ++ ")"

type TableGen = State Int

i32 :: TableGen (SDNode L I32)
i32 = SDPatLeaf "" <$> next_int

i16 :: TableGen (SDNode L I16)
i16 = SDPatLeaf "" <$> next_int

i8 :: TableGen (SDNode L I8)
i8 = SDPatLeaf "" <$> next_int

next_int = State.get >>= \n -> State.modify (+ 1) >> return n
