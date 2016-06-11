-- uni-typed everything

module SDNode where

import Data.List (intercalate)

data Operand = Reg Int | Imm Int | BasicBlock | Addr | Unknown

data CondCode
    = SETNE | SETEQ | SETGT | SETGE | SETLT | SETLE
    | SETUNE | SETUEQ | SETUGT | SETUGE | SETULT | SETULE
    deriving Show

data SDNode
    = SDNode String [SDNode]
    | SDConst Int Int
    | SDCondCode CondCode
    | SDLeaf Operand String Int

instance Show SDNode where
    show (SDNode opname ns) = op_print opname ns
    show (SDConst width n) = unwords [sd_show (Reg width), show n]
    show (SDCondCode cc) = show cc
    show (SDLeaf ty "" opid) = sd_show ty ++ ": $op" ++ show opid
    show (SDLeaf _ wrap opid) = wrap ++ ": $op" ++ show opid

class HasLeaves t where is_leaf :: t -> Bool
instance HasLeaves SDNode where
    is_leaf SDNode{} = False
    is_leaf SDConst{} = False
    is_leaf _ = True

op_print :: (HasLeaves t, Show t) => String -> [t] -> String
op_print name operands = unwords [name, comma_join $ map paren_show operands]
    where paren_show n = if is_leaf n then show n else "(" ++ show n ++ ")"

comma_join = intercalate ", "

sd_show (Reg width) = "i" ++ show width
sd_show (Imm width) = "i" ++ show width ++ "imm"
sd_show BasicBlock = "bb"
sd_show Addr = "addr"
sd_show Unknown = "unknown"

binop :: String -> SDNode -> SDNode -> SDNode
binop opname lhs rhs = SDNode opname [lhs, rhs]

add         = binop "add"
sub         = binop "sub"
mul         = binop "mul"
sdiv        = binop "sdiv"
udiv        = binop "udiv"
and         = binop "and"
or          = binop "or"
xor         = binop "xor"
addc        = binop "addc"
adde        = binop "adde"
subc        = binop "subc"
sube        = binop "sube"
smin        = binop "smin"
smax        = binop "smax"
umin        = binop "umin"
umax        = binop "umax"

srl         = binop "srl"
sra         = binop "sra"
shl         = binop "shl"
rotl        = binop "rotl"
rotr        = binop "rotr"

coerce width node = SDNode (sd_show $ Reg width) [node]

ext exttype width node = coerce width $ SDNode exttype [node]

[sext, zext, anyext] = map ext $ words "sext zext anyext"

trunc width node = coerce width $ SDNode "trunc" [node]

build_pair  = binop "build_pair"

load addr = SDNode "load" [addr]
store val addr = SDNode "store" [val, addr]

brcc cc l r jmp = SDNode "brcc" [SDCondCode cc, l, r, jmp]

br jmp = SDNode "br" [jmp]

selectcc l r tval fval cc = SDNode "selectcc" [l, r, tval, fval, SDCondCode cc]

const8 = SDConst 8
const16 = SDConst 16
const32 = SDConst 32

condcodes = zip [SETNE, SETEQ, SETGT, SETGE, SETLT, SETLE]
                [SETUNE, SETUEQ, SETUGT, SETUGE, SETULT, SETULE]
