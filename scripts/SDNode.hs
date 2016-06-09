{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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
    SDUnOp :: UnOpz a b => String -> SDNode l a -> SDNode N b
    SDBinOp :: BinOpz a b c => String -> SDNode k a -> SDNode l b -> SDNode N c
    SDShiftOp :: ShiftOpz a b c => String -> SDNode k a -> SDNode l b
              -> SDNode N c
    SDTruncOp :: SDGT a b => SDNode l a -> SDNode N b
    SDExtOp :: SDGT b a => String -> SDNode l a -> SDNode N b
    SDBuildPair :: SDGT b a => SDNode k a -> SDNode l a -> SDNode N b
    SDLoadOp :: SDIsInt a => SDNode l SDPtr -> SDNode N a
    SDStoreOp :: SDIsInt a => SDNode k a -> SDNode l SDPtr -> SDNode N SDUnit
    SDSelectCC :: SDNode h a -> SDNode j a -> SDNode k b -> SDNode l b
               -> CondCode -> SDNode N b
    SDBrCC :: CondCode -> SDNode j a -> SDNode k a -> SDNode l BasicBlock
           -> SDNode N SDUnit
    SDBr :: SDNode l BasicBlock -> SDNode N SDUnit
    SDCoerce :: TypeShow a => SDNode l a -> SDNode l a
    SDConst :: Integer -> SDNode N a
    SDPatLeaf :: TypeShow a => String -> Int -> SDNode L a

-- Operand kind
data SDKind = SDI Nat | Imm Nat | SDPtr | SDUnit | BasicBlock

data CondCode
    = SETOEQ | SETOGT | SETOGE | SETOLT | SETOLE | SETONE | SETO
    | SETUO | SETUEQ | SETUGT | SETUGE | SETULT | SETULE | SETUNE
    | SETEQ | SETGT | SETGE | SETLT | SETLE | SETNE
    deriving (Show, Eq)

type I32    = SDI 32
type I16    = SDI 16
type I8     = SDI 8

class TypeShow z => SDIsInt z

instance KnownNat n => SDIsInt (SDI n)
instance KnownNat n => SDIsInt (Imm n)

type family SDKindWidth a where
    SDKindWidth (SDI n) = n
    SDKindWidth (Imm n) = n

type family SameWidth a b where
    SameWidth a b = CmpNat (SDKindWidth a) (SDKindWidth b) ~ EQ

type family SDGT a b where
    SDGT m n = CmpNat (SDKindWidth m) (SDKindWidth n) ~ GT

data ShowWrap = forall a. Show a => E_ a

-- TODO: `show` expects a haskell-readable output. consider switching to a
-- different typeclass.
instance Show ShowWrap where
    show (E_ inner) = show inner

instance Show (SDNode l a) where
    show (SDUnOp opname n) = op_print opname [E_ n]
    show (SDBinOp opname l r) = op_print opname [E_ l, E_ r]
    show (SDShiftOp opname src sh) = op_print opname [E_ src, E_ sh]
    show (SDTruncOp n) = op_print "trunc" [E_ n]
    show (SDExtOp extttype n) = op_print extttype [E_ n]
    show (SDBuildPair l r) = op_print "build_pair" [E_ l, E_ r]
    show (SDLoadOp p) = op_print "load" [E_ p]
    show (SDStoreOp val p) = op_print "store" [E_ val, E_ p]
    show (SDSelectCC l r t f cc) =
        op_print "selectcc" [E_ l, E_ r, E_ t, E_ f, E_ cc]
    show (SDBrCC cc l r bb) = op_print "brcc" [E_ cc, E_ l, E_ r, E_ bb]
    show (SDBr bb) = op_print "br" [E_ bb]
    show (SDCoerce n) = op_print (type_show (Proxy :: Proxy a)) [E_ n]
    show (SDConst n) = show n
    show (SDPatLeaf "" leafid) =
        type_show (Proxy :: Proxy a) ++ ": $op" ++ show leafid
    show (SDPatLeaf wrapper leafid) = wrapper ++ ": $op" ++ show leafid

class TypeShow t where
    type_show :: Proxy t -> String
    mach_type_show :: Proxy t -> String

instance KnownNat n => TypeShow (SDI n) where
    type_show _ = "i" ++ show (natVal (Proxy :: Proxy n))
    mach_type_show _ = "reg" ++ show (natVal (Proxy :: Proxy n))

instance KnownNat n => TypeShow (Imm n) where
    type_show _ = "i" ++ show (natVal (Proxy :: Proxy n)) ++ "imm"
    mach_type_show = type_show

instance TypeShow BasicBlock where
    type_show _ = "bb"
    mach_type_show _ = "bb"

instance TypeShow SDPtr where
    type_show _ = "addr"
    mach_type_show _ = "addr"

instance TypeShow a => Num (SDNode N a) where
    fromInteger = SDConst
    (+) = undefined
    (*) = undefined
    abs = undefined
    signum = undefined
    negate = undefined

op_print :: Show a => String -> [a] -> String
op_print opname [] = parens opname
op_print opname xs = parens $ opname ++ " " ++ comma_join (map show xs)

comma_join :: [String] -> String
comma_join = List.intercalate ", "

parens xs = "(" ++ xs ++ ")"

type ShiftOpz d l r = (SDIsInt d, SDIsInt l, SDIsInt r, SameWidth d l)

type BinOpz d l r = (ShiftOpz d l r, SameWidth d r)

type UnOpz d s = (SDIsInt d, SDIsInt s, SameWidth d s)

add, sub, mul, sdiv, udiv, and, or, xor, addc, adde, subc, sube, smin, smax,
    umin, umax :: BinOpz a b c => SDNode k a -> SDNode l b -> SDNode N c
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

srl, sra, shl, rotl, rotr :: ShiftOpz a b c => SDNode k a -> SDNode l b
                          -> SDNode N c
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

store :: SDIsInt a => SDNode k a -> SDNode l SDPtr-> SDNode N SDUnit
store       = SDStoreOp

-- helpers

as8 :: SDNode N I8 -> SDNode N I8
as8         = SDCoerce

as16 :: SDNode N I16 -> SDNode N I16
as16        = SDCoerce

as32 :: SDNode N I32 -> SDNode N I32
as32        = SDCoerce

sext8, zext8, anyext8 :: SDGT I8 a => SDNode l a -> SDNode N I8
sext8       = SDExtOp "sext"
zext8       = SDExtOp "zext"
anyext8     = SDExtOp "anyext"

sext16, zext16, anyext16 :: SDGT I16 a => SDNode l a -> SDNode N I16
sext16      = SDExtOp "sext"
zext16      = SDExtOp "zext"
anyext16    = SDExtOp "anyext"

sext32, zext32, anyext32 :: SDGT I32 a => SDNode l a -> SDNode N I32
sext32      = SDExtOp "sext"
zext32      = SDExtOp "zext"
anyext32    = SDExtOp "anyext"

trunc8 :: SDGT a I8 => SDNode l a -> SDNode N I8
trunc8      = SDTruncOp

trunc16 :: SDGT a I16 => SDNode l a -> SDNode N I16
trunc16     = SDTruncOp

trunc32 :: SDGT a I32 => SDNode l a -> SDNode N I32
trunc32     = SDTruncOp
