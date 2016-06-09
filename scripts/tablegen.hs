{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

import SDNode
import qualified MachNode
import MachNode hiding (L)
import GHC.TypeLits (KnownNat)
import Data.Proxy (Proxy(Proxy))
import qualified Control.Monad.Trans.Writer as Writer
import qualified Control.Monad.Trans.State as State
import TH
import Decls

type TableGen = MachGen

i32 :: TableGen (SDNode L I32)
i32 = new_sd_op

i16 :: TableGen (SDNode L I16)
i16 = new_sd_op

i8 :: TableGen (SDNode L I8)
i8 = new_sd_op

bb :: TableGen (SDNode L BasicBlock)
bb = new_sd_op

i8imm_31 :: TableGen (SDNode L I8)
i16imm_31 :: TableGen (SDNode L I16)
i32imm_31 :: TableGen (SDNode L I32)
i8imm_31 = wrapped_sd_op "i8imm_31"
i16imm_31 = wrapped_sd_op "i16imm_31"
i32imm_31 = wrapped_sd_op "i32imm_31"

i8imm_255 :: TableGen (SDNode L I8)
i16imm_255 :: TableGen (SDNode L I16)
i32imm_255 :: TableGen (SDNode L I32)
i8imm_255   = wrapped_sd_op "i8imm_255"
i16imm_255  = wrapped_sd_op "i16imm_255"
i32imm_255  = wrapped_sd_op "i32imm_255"

i16imm_65535 :: TableGen (SDNode L I16)
i32imm_65535 :: TableGen (SDNode L I32)
i16imm_65535  = wrapped_sd_op "i16imm_65535"
i32imm_65535  = wrapped_sd_op "i32imm_65535"

i8imm_neg_within_255  :: TableGen (SDNode L I8)
i16imm_neg_within_255 :: TableGen (SDNode L I16)
i32imm_neg_within_255 :: TableGen (SDNode L I32)
i8imm_neg_within_255  = wrapped_sd_op "i8imm_neg_within_255"
i16imm_neg_within_255 = wrapped_sd_op "i16imm_neg_within_255"
i32imm_neg_within_255 = wrapped_sd_op "i32imm_neg_within_255"

addr :: TableGen (SDNode L SDPtr)
addr = new_sd_op

regaddr :: TableGen (SDNode L SDPtr)
regaddr = wrapped_sd_op "regaddr"

data Pattern
    = forall l lm k. Pattern (SDNode l k) (MachNode lm k)
    | forall lm k. NullPat (MachNode lm k)

instance Show Pattern where
    show (Pattern sdn mnode) = unlines [
          "def Pat<"
        , indent 4 (show sdn) ++  ","
        , indent 4 $ show mnode
        , ">;"
        ]
    show NullPat{} = undefined

braces xs = "{" : xs ++ ["}"]

indent n = (replicate n ' ' ++)

semicoloned = map (++ ";")

lets = map ("let " ++)

quoted xs = '\"' : xs ++ "\""

instrfold :: Monoid b
          => (forall ins dty. C ins dty => MachNode (I ins) dty -> b)
          -> MachNode l a -> b
instrfold f n@(MachInstr _ _ _ subnodes) =
        f n `mappend` mconcat (map recurse subnodes)
    where recurse (M_ x) = instrfold f $ x
instrfold f (EXTRACT_SUBREG n _) = instrfold f n
instrfold f (COPY_TO_REGCLASS n) = instrfold f n
instrfold f (SUBREG_TO_REG n _) = instrfold f n
instrfold _ _ = mempty

idk :: forall ins dty. C ins dty => MachNode (I ins) dty -> [String]
idk z@(MachInstr attrs llname asmpr _) =
        [unlines $ header : braces (map (indent 4) . lets $ semicoloned body)]
    where
    header = "def " ++ llname ++ " : Instruction"
    body =
        [ "Pattern = []"  -- pats generated separately
        , "InOperandList = " ++ parens (comma_join inops)
        , "OutOperandList = " ++ parens (maybe "" id outop)
        , "AsmString = " ++ quoted (get_asm_string z)
        -- TODO: attrs
        ]
    (inops, outop) = get_operands z

a ->> b = return $ Pattern a b

-- arithmetic
zext_is_free :: (forall l r u v w. BinOpz u v w => SDNode l u -> SDNode r v
             -> SDNode SDNode.N w)
             -> (forall l r u v w. (KnownNat u, KnownNat v, KnownNat w)
             => MachNode l (SDI u) -> MachNode r (SDI v)
             -> MachNode (I '[SDI u, SDI v]) (SDI w)) -> Int -> Int
             -> TableGen Pattern
zext_is_free sdbinop machop lwidth rwidth = case (lwidth, rwidth) of
    (8, 16) -> do
        l <- i8
        r <- i16
        sdbinop (zext16 l) r ->> machop (m l) (m r)
    (8, 32) -> do
        l <- i8
        r <- i32
        sdbinop (zext32 l) r ->> machop (m l) (m r)
    (16, 32) -> do
        l <- i16
        r <- i32
        sdbinop (zext32 l) r ->> machop (m l) (m r)
    (16, 8) -> do
        l <- i16
        r <- i8
        sdbinop l (zext16 r) ->> machop (m l) (m r)
    (32, 8) -> do
        l <- i32
        r <- i8
        sdbinop l (zext32 r) ->> machop (m l) (m r)
    (32, 16) -> do
        l <- i32
        r <- i16
        sdbinop l (zext32 r) ->> machop (m l) (m r)
