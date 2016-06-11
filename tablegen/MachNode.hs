module MachNode where

import Data.List (intercalate)
import SDNode (HasLeaves(is_leaf), SDNode(SDLeaf), Operand(..), op_print,
               comma_join, sd_show)
import Prelude hiding (unlines)

data Instruction
    = Instruction
    { out :: Maybe Operand
    , ins :: [Operand]
    , asm_format :: [MachNode] -> MachNode -> String
    , llvm_name :: String
    , attrs :: [IAttr]
    }

data IAttr
    = IsReturn Bool | IsBranch Bool | IsTerminator Bool | HasSideEffects Bool
    | IsPseudo Bool | IsMoveImm Bool | Uses [String] | Defs [String]
    | UsesCustomInserter Bool | IsBarrier Bool | IsRematerializable Bool
    | IsCall Bool

data MachNode
    = MachInstr Instruction [MachNode]
    | MNode String [MachNode]
    | MLeaf SDNode  -- SDLeaf
    | MMisc String

newtype SubIndex = SubIndex (Int, Int)

class ToMLeaf t where m :: t -> MachNode
instance ToMLeaf SDNode where
    m z@SDLeaf{} = MLeaf z
    m _ = error $ "don't feed non-leaf sdnode into machnode."
instance ToMLeaf MachNode where m = id
instance ToMLeaf SubIndex where
    m (SubIndex (m, n)) = MMisc $ "sub_" ++ show m ++ "_" ++ show n

instance HasLeaves MachNode where
    is_leaf MachInstr{} = False
    is_leaf MNode{} = False
    is_leaf _ = True

instance Show MachNode where
    show (MachInstr instr rest) = op_print (llvm_name instr) rest
    show (MNode opname rest) = op_print opname rest
    show (MLeaf (SDLeaf ty "" opid)) = mach_show ty ++ ": $op" ++ show opid
    show (MLeaf (SDLeaf _ wrap opid)) = wrap ++ ": $op" ++ show opid
    show (MLeaf n) = error $ "expected a leaf node, got " ++ show n
    show (MMisc str) = str

mach_show (Reg width) = "reg" ++ show width
mach_show (Imm width) = "i" ++ show width ++ "imm"
mach_show BasicBlock = "jump_target"
mach_show n = sd_show n

opshow :: MachNode -> String
opshow (MLeaf (SDLeaf _ _ opid)) = "$op" ++ show opid
opshow n = error $ "opshow called on non-leaf node: " ++ show n

-- generate llvm def
gen_def :: Instruction -> String
gen_def (Instruction outop inops asmp llname attrs) =
        unlines [header, braced body]
    where
    header = "def " ++ llname ++ " : Instruction"
    body = unlines . map (indent 4) . map ("let " ++) . semicoloned $ [
          "OutOperandList = (outs " ++ maybe "" show outop' ++ ")"
        , "InOperandList = (ins " ++ comma_join (map show inops') ++ ")"
        , "Pattern = []"
        , "AsmString = " ++ quoted (asmp inops' $ maybe undefined id outop')
        ]
        ++ map (join . attr_show) attrs
    join (k, v) = unwords [k, "=", v]
    inops' = zipWith (\o n -> MLeaf $ SDLeaf o "" n) inops [0..]
    outop' = fmap (\z -> MLeaf $ SDLeaf z "" outid) outop
        where outid = length inops

attr_show (IsReturn n) = ("isReturn", to_bit n)
attr_show (IsBranch n) = ("isBranch", to_bit n)
attr_show (IsTerminator n) = ("isTerminator", to_bit n)
attr_show (HasSideEffects n) = ("hasSideEffects", to_bit n)
attr_show (IsPseudo n) = ("isPseudo", to_bit n)
attr_show (IsMoveImm n) = ("isMoveImm", to_bit n)
attr_show (UsesCustomInserter n) = ("usesCustomInserter", to_bit n)
attr_show (Uses regs) = ("Uses", "[" ++ comma_join regs ++ "]")
attr_show (Defs regs) = ("Defs", "[" ++ comma_join regs ++ "]")
attr_show (IsBarrier n) = ("isBarrier", to_bit n)
attr_show (IsRematerializable n) = ("isReMaterializable", to_bit n)
attr_show (IsCall n) = ("isCall", to_bit n)

to_bit False = "0"
to_bit True = "1"

semicoloned = map (++ ";")

indent n = (replicate n ' ' ++)

braced xs = unlines ["{", xs, "}"]

quoted xs = "\"" ++ xs ++ "\""

parens xs = "(" ++ xs ++ ")"

unlines = intercalate "\n"

-- helper nodes
subidx m n = SubIndex (m, n)

subreg_to_reg destwidth src subi = MNode "SUBREG_TO_REG" [idx, m src, m subi]
    where idx = MNode ("i" ++ show destwidth) [MMisc "0"]

extract_subreg src subi = MNode "EXTRACT_SUBREG" [m src, m subi]

insert_subreg reg subreg subi = MNode "INSERT_SUBREG" [m reg, m subreg, m subi]

-- IMPLICIT_DEF needs its own set of parens
implicit_def n = MNode ("i" ++ show n) [MMisc "(IMPLICIT_DEF)"]
