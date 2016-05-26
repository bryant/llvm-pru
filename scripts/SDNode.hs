{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module SDNode where

import qualified Data.List as List
import Data.Proxy (Proxy(Proxy))
import GHC.TypeLits (Symbol, Nat, CmpNat, KnownNat, natVal)
import Prelude hiding
    ( add, sub, mul, sdiv, udiv, and, or, xor, addc, adde, subc, sube, smin
    , smax, umin, umax
    )

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
    SDSelectCC :: SDNode h a -> SDNode j a -> SDNode k b -> SDNode l b
               -> CondCode -> SDNode N b
    SDBrCC :: CondCode -> SDNode j a -> SDNode k a -> SDNode l BasicBlock
           -> SDNode N SDUnit
    SDBr :: SDNode l BasicBlock -> SDNode N SDUnit
    SDPatLeaf :: TypeShow a => String -> Int -> SDNode L a

-- Operand kind
data SDKind = SDI Nat | Imm Nat | SDPtr | SDUnit | BasicBlock

data CondCode
    = SETOEQ | SETOGT | SETOGE | SETOLT | SETOLE | SETONE | SETO
    | SETUO | SETUEQ | SETUGT | SETUGE | SETULT | SETULE | SETUNE
    | SETEQ | SETGT | SETGE | SETLT | SETLE | SETNE
    deriving (Show, Eq)

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

op_print :: Show a => String -> [a] -> String
op_print opname [] = opname
op_print opname xs = opname ++ " " ++ comma_join (map show xs)

comma_join :: [String] -> String
comma_join = List.intercalate ", "

parens xs = "(" ++ xs ++ ")"

add, sub, mul, sdiv, udiv :: SDIsInt a => SDNode k a -> SDNode l a -> SDNode N a
and, or, xor, addc, adde :: SDIsInt a => SDNode k a -> SDNode l a -> SDNode N a
subc, sube, smin, smax :: SDIsInt a => SDNode k a -> SDNode l a -> SDNode N a
umin, umax :: SDIsInt a => SDNode k a -> SDNode l a -> SDNode N a
add         = SDBinOp "add"
sub         = SDBinOp "sub"
mul         = SDBinOp "mul"
sdiv        = SDBinOp "sdiv"
udiv        = SDBinOp "udiv"
and         = SDBinOp "and"
or          = SDBinOp "or"
xor         = SDBinOp "xor"
addc        = SDBinOp "addc"
adde        = SDBinOp "adde"
subc        = SDBinOp "subc"
sube        = SDBinOp "sube"
smin        = SDBinOp "smin"
smax        = SDBinOp "smax"
umin        = SDBinOp "umin"
umax        = SDBinOp "umax"

srl, sra, shl, rotl, rotr :: (SDIsInt a, SDIsInt b) => SDNode k a -> SDNode l b
                          -> SDNode N a
srl         = SDShiftOp "srl"
sra         = SDShiftOp "sra"
shl         = SDShiftOp "shl"
rotl        = SDShiftOp "rotl"
rotr        = SDShiftOp "rotr"

sext, zext, anyext :: SDGT b a => SDNode l a -> SDNode N b
sext        = SDExtOp "sext" 
zext        = SDExtOp "zext"
anyext      = SDExtOp "anyext"

trunc :: SDGT a b => SDNode l a -> SDNode N b
trunc       = SDTruncOp

build_pair :: SDGT b a => SDNode k a -> SDNode l a -> SDNode N b
build_pair  = SDBuildPair

load :: SDIsInt a => SDNode l SDPtr -> SDNode N a
load        = SDLoadOp

store :: SDIsInt a => SDNode k SDPtr -> SDNode l a -> SDNode N SDUnit
store       = SDStoreOp
