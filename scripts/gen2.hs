import Data.List (intercalate, splitAt)
import Numeric (showHex)
import System.Environment (getArgs)
import Data.Char (toUpper)

data Operand a = Operand { op_label :: String, op_op :: a }

-- this maps one-to-one to the sdag node types. perhaps they can be merged.
data MachineOp = Reg Int | Imm Int Int | JumpTarg

mop_width (Imm w _) = w
mop_width (Reg n) = n
mop_width JumpTarg = error "cannot take width of jump targ, which is a label"

-- it's fucking stupid that TargetConstant/Constant have widths
data DagOp = I Int | ImmLeaf Int Int | UnboundedImm Int | BasicBlock

type DOp = Operand DagOp

type MOp = Operand MachineOp

data CondCode
    = SETNE | SETEQ | SETGT | SETGE | SETLT | SETLE
                    | SETUGT | SETUGE | SETULT | SETULE
    deriving Show

data SDNode
    = Brcc CondCode SDNode SDNode SDNode  -- lhs, rhs, bb
    | Br SDNode  -- bb
    | Selectcc SDNode SDNode SDNode SDNode CondCode
    | ZExt SDNode
    | AnyExt SDNode
    | Trunc SDNode
    | Set (Operand DagOp) SDNode
    | CoerceTo Int SDNode
    | SDBinOp String SDNode SDNode
    | SDNot SDNode
    | SDOp (Operand DagOp)  -- | never occurs at root

instance Functor Operand where
    fmap f oper = oper { op_op = f $ op_op oper }

instance Show DagOp where
    show (I n) = "i" ++ show n
    show (ImmLeaf width limit) = "i" ++ show width ++ "imm_" ++ show limit
    show (UnboundedImm _ ) = "imm"
    show BasicBlock = "bb"

instance Show MachineOp where
    show (Reg n) = "reg" ++ show n
    show (Imm n _) = "i" ++ show n ++ "imm"
    show JumpTarg = "jump_target"

instance Show SDNode where
    show (Brcc cc lhs rhs bb) = dag "brcc" $ show cc : map show [lhs, rhs, bb]
    show (Br bb) = dag "br" [show bb]
    show (Selectcc lhs rhs tval fval cc) = dag "selectcc" $
                                    map show [lhs, rhs, tval, fval] ++ [show cc]
    show (ZExt n) = dag "zext" [show n]
    show (AnyExt n) = dag "anyext" [show n]
    show (Trunc n) = dag "trunc" [show n]
    show (Set oper ns) = dag "set" [show oper, show ns]
    show (CoerceTo width n) = parens .unwords $ ["i" ++ show width, show n]
    show (SDBinOp s n n') = dag s [show n, show n']
    show (SDNot n) = dag "not" [show n]
    show (SDOp oper) = show oper

instance Show a => Show (Operand a) where
    show (Operand "" op) = show op
    show (Operand label op) = show op ++ ": " ++ label

data ExtraAttr = IsReturn Bool | IsBranch Bool | IsTerminator Bool
    | HasSideEffects Bool | IsPseudo Bool | IsMoveImm Bool | Uses [String]
    | Defs [String] | UsesCustomInserter Bool | IsBarrier Bool
    | IsRematerializable Bool

-- TODO: consider replacing this entire enterprise with something that's
-- remotely fucking typesafe
data Instruction
    = Instruction
    { def_name :: String
    , out_ops :: [MachineOp]
    , in_ops :: [MachineOp]
    , mnemonic :: [MOp] -> [MOp] -> String
    , patterns :: [MOp] -> {- [MOp] -> -} [SDNode]
    -- ^ TODO: what do when multi-effect instr pat?
    , extra_attr :: [ExtraAttr]
    }

-- | `def add : Instruction<...> => (add reg, reg, ...)`
as_dag instr =  -- note that only input ops are present in the dag
    dag (def_name instr) . map show_hack . fst $ mk_labels (in_ops instr) []
    where
    -- why? because fuck tablegen, that's why
    -- without this, i.e. if imm mach operands printed as iXimm in patterns,
    -- then EmitConvertToTarget is never emitted.
    show_hack (Operand lab (Imm width limit)) = op ++ ": " ++ lab
        where op = "i" ++ show width ++ "imm_" ++ show limit
    show_hack machineop = show machineop

-- | `def add : Instruction<...>`
decl instr =
        "def " ++ def_name instr ++ " : Instruction " ++
        braces (indent 4 . map letfmt $ defs ++ map extfmt (extra_attr instr))
    where
    (ins, outs) = mk_labels (in_ops instr) (out_ops instr)
    defs = [
          ("OutOperandList", dag "outs" $ map show outs)
        , ("InOperandList", dag "ins" $ map show ins)
        , ("AsmString", quoted $ mnemonic instr outs ins)
        , ("Pattern", "[]")  -- | patterns defined separately
        ]
    letfmt (k, v) = unwords ["let", k, "=", v, ";"]
    indent n = unlines . map ((replicate n ' ') ++)

pat_decls instr = map print_pat $ patterns instr ins
    where
    (ins, _) = mk_labels (in_ops instr) []
    print_pat pat = "def : Pat<" ++ show pat ++ ", " ++ as_dag instr ++ ">;"

braces xs = "{\n" ++ xs ++ "}\n"

quoted xs = "\"" ++ xs ++ "\""

parens s = concat ["(", s, ")"]

commasep = intercalate ", "

extfmt (IsReturn n) = ("isReturn", to_bit n)
extfmt (IsBranch n) = ("isBranch", to_bit n)
extfmt (IsTerminator n) = ("isTerminator", to_bit n)
extfmt (HasSideEffects n) = ("hasSideEffects", to_bit n)
extfmt (IsPseudo n) = ("isPseudo", to_bit n)
extfmt (IsMoveImm n) = ("isMoveImm", to_bit n)
extfmt (UsesCustomInserter n) = ("usesCustomInserter", to_bit n)
extfmt (Uses regs) = ("Uses", "[" ++ commasep (map show regs) ++ "]")
extfmt (Defs regs) = ("Defs", "[" ++ commasep (map show regs) ++ "]")
extfmt (IsBarrier n) = ("isBarrier", to_bit n)
extfmt (IsRematerializable n) = ("isReMaterializable", to_bit n)

to_bit :: Bool -> String
to_bit = show . fromEnum

mk_labels :: [MachineOp] -> [MachineOp] -> ([MOp], [MOp])
mk_labels ins outs = splitAt (length ins) . zipWith Operand tags $ ins ++ outs
    where
    tags = map (("$p" ++) . show) [0..]

dag pref opers = parens $ pref ++ " " ++ commasep opers

reg = map Reg [8, 16, 32]

imm = map Imm [8, 16, 32]

r_r_r = [(d, l, r) | d <- reg, l <- reg, r <- reg]

r_r_op n = r_r_r ++ [(d, l, r n) | d <- reg, l <- reg, r <- imm]

r_r = [(d, l) | d <- reg, l <- reg]

r_imm n = [(d, i n) | d <- reg, i <- imm]

r_op n = r_r ++ r_imm n

to_sdty (Reg n) = I n
to_sdty (Imm width n) = ImmLeaf width n
to_sdty JumpTarg = BasicBlock

-- for instance, match both:
--         op--+--lhs
--         |   |
-- dwidth--+   |        dest = trunc $ lhs `op` (ext rhs)
--             |
--             +--rhs
-- or
--             +--lhs
--             |
-- dwidth--op--+        dest = (trunc lhs) `op` (ext rhs)
--             |
--             +--rhs
-- update: turns out, llvm always combines into the second form. so this entire
-- mess wasn't even necessary. keeping it, just in case.
ext_or_trunc ext dwidth iwidths =
    [(map (`ext_or_trunc_to` opw) iwidths, opw `ext_or_trunc_to` dwidth)]
    where
    opw = maximum $ dwidth : iwidths
    iwidth `ext_or_trunc_to` op_width
        | iwidth < op_width = CoerceTo op_width . ext
        | iwidth > op_width = CoerceTo op_width . Trunc
        | otherwise = id

ext_largest ext iwidths = map (`ext_to` maximum iwidths) iwidths
    where
    k `ext_to` n
        | k < n = CoerceTo n . ext
        | otherwise = id

mangled pref operands = pref ++ concatMap (('_' :) . show) operands

mnem pref outs ins = map toUpper pref ++ " " ++
                     commasep (map op_label $ outs ++ ins)

pats op dest ins@(_:_) =
        [ds . op $ zipWith ($) cs sdops 
            | ext <- [AnyExt, ZExt],
            (cs, ds) <- ext_or_trunc ext (mop_width dest) op_widths]
    where
    op_widths = map (mop_width . op_op) ins
    sdops = map (SDOp . fmap to_sdty) ins
pats _ _ _ = error "pats only used for instructions with at least one operand"

br_pats JumpTarg [jmptarget] = [Br . SDOp $ fmap to_sdty jmptarget]
br_pats _ _ = []

simple_br pref mpref cc (lhs, rhs) =
    Instruction (mangled pref [lhs, rhs]) [] [JumpTarg, lhs, rhs] (mnem mpref)
                (qb_pats cc)
                [IsBranch True, IsTerminator True]
    where
    qb_pats cc ins@[jt, l, r] = [Brcc cc l' r' (SDOp $ fmap to_sdty jt)]
        where
        exts = ext_largest ZExt $ map (mop_width . op_op) [l, r]
        [l', r'] = zipWith ($) exts $ map (SDOp . fmap to_sdty) [l, r]

alu_bin_op pref mpref op (dest, lhs, rhs) =
    Instruction (mangled pref [dest, lhs, rhs]) [dest] [lhs, rhs] (mnem mpref)
                (pats op' dest) attrs
    where
    attrs | Imm _ _ <- rhs = [IsRematerializable True] | otherwise = []
    op' [l, r] = op l r

alu_un_op p mp op (dest, src) =
    Instruction (mangled p [dest, src]) [dest] [src] (mnem mp) (pats op' dest)
                []
    where op' [l] = op l

pru_add = map (alu_bin_op "pru_add" "add" (SDBinOp "add")) $ r_r_op 255
pru_sub = map (alu_bin_op "pru_sub" "sub" (SDBinOp "sub")) $ r_r_op 255
pru_rsb = map (alu_bin_op "pru_rsc" "rsc" (flip $ SDBinOp "sub")) $ r_r_op 255

pru_lsl = map (alu_bin_op "pru_lsl" "lsl" (SDBinOp "shl")) $ r_r_op 31
pru_lsr = map (alu_bin_op "pru_lsr" "lsr" (SDBinOp "srl")) $ r_r_op 31

pru_and = map (alu_bin_op "pru_and" "and" (SDBinOp "and")) $ r_r_op 255
pru_or = map (alu_bin_op "pru_or" "or" (SDBinOp "or")) $ r_r_op 255
pru_xor = map (alu_bin_op "pru_xor" "xor" (SDBinOp "xor")) $ r_r_op 255

pru_not = map (alu_un_op "pru_not" "not" SDNot) $ r_r

pru_min = map (alu_bin_op "pru_min" "min" (SDBinOp "umin")) $ r_r_op 255
pru_max = map (alu_bin_op "pru_max" "max" (SDBinOp "umax")) $ r_r_op 255

{-
clr
set
lmbd
-}

pru_nop =
    [Instruction (mangled ("pru_nop" ++ hex nopnum) [d, l, r]) [d] [l, r]
                 (mnem $ "nop" ++ hex nopnum)
                 (const []) [HasSideEffects False]
        | nopnum <- [0..15], (d, l, r) <- r_r_op 255]
    where hex = flip showHex ""

-- reg-imm moves require inline pats; defined in .td
pru_mov =
    [Instruction (mangled "pru_mov" [d, s]) [d] [s] (mnem "mov")
                 -- equitype reg-reg moves defined in PRUInstrInfo::copyPhysReg
                 (if mop_width d > mop_width s then pats head d else const [])
                 [IsPseudo True]
        | (d, s) <- r_r]

-- ldi defined with inline pats.

-- really really weird; half-implemented by ti cc and pasm
-- mvib reg reg
-- mviw
-- mvid

--pru_lbbo = [Instruction (mangled "pru_lbbo" [d, offset]) 
{-
lbbo reg rn (op 8) (imm 124) / reg rn (op 8) bn
sbbo

lbco reg cn (op 8) (imm 124 / bn)
sbco

'zero (imm 123 / reg) (imm 124)
'fill

xin (imm 253) reg (imm 124 / bn)
xout
xchg

sxin
sxout
sxchg
-}

pru_jmp = [Instruction (mangled "pru_jmp" [jt]) [] [jt] (mnem "jmp")
                (br_pats jt) [IsBranch True, IsTerminator True, IsBarrier True]
          | jt <- JumpTarg : reg]

{-
pru_jal 

jal reg (reg / label)
'call (reg / label)
'ret
-}

pru_qbgt = map (simple_br "pru_qbgt" "qbgt" SETUGT) $ r_op 255
pru_qbge = map (simple_br "pru_qbge" "qbge" SETUGE) $ r_op 255
pru_qblt = map (simple_br "pru_qblt" "qblt" SETULT) $ r_op 255
pru_qble = map (simple_br "pru_qble" "qble" SETULE) $ r_op 255
pru_qbeq = map (simple_br "pru_qbeq" "qbeq" SETEQ) $ r_op 255
pru_qbne = map (simple_br "pru_qbne" "qbne" SETNE) $ r_op 255

pru_qba =
    [Instruction "pru_qba" [] [JumpTarg] (mnem "qba") (br_pats JumpTarg)
                 [IsBranch True, IsTerminator True]]

-- TODO: complex patterns for these qbbs maps to (brcc (and something))
pru_qbbs =
    [Instruction (mangled "pru_qbbs" [src, bit]) [] [JumpTarg, src, bit]
                 (mnem "qbbs") (const []) [IsBranch True, IsTerminator True]
        | (src, bit) <- r_op 31]

pru_qbbc =
    [Instruction (mangled "pru_qbbc" [src, bit]) [] [JumpTarg, src, bit]
                 (mnem "qbbc") (const []) [IsBranch True, IsTerminator True]
        | (src, bit) <- r_op 31]

pru_wbs =
    [Instruction (mangled "pru_wbs" [src, bit]) [] [src, bit] (mnem "wbs")
                 (const []) [IsBranch True, IsTerminator True, IsPseudo True]
        | (src, bit) <- r_op 255]

pru_wbc =
    [Instruction (mangled "pru_wbc" [src, bit]) [] [src, bit] (mnem "wbc")
                 (const []) [IsBranch True, IsTerminator True, IsPseudo True]
        | (src, bit) <- r_op 255]

{-
hlt
slp
-}

pru_loop =
    [Instruction (mangled "pru_loop" [countreg]) [] [JumpTarg, countreg]
                 (mnem "loop") (const []) [IsBranch True, IsTerminator True]
        | countreg <- Imm 8 255 : reg]
pru_iloop =
    [Instruction (mangled "pru_iloop" [countreg]) [] [JumpTarg, countreg]
                 (mnem "iloop") (const []) [IsBranch True, IsTerminator True]
        | countreg <- Imm 8 255 : reg]

(selectccs, corresponding_qb) = unzip
    [(Instruction def [val] [lhs, rhs, val, val] (\_ _ -> def) (selectpats cc)
                 [UsesCustomInserter True],
      mangled ("pru_qb" ++ showcond cc) [lhs, rhs])
        | cc <- condcodes, (lhs, rhs) <- r_op 255, val <- reg,
        let def = mangled ("pru_select" ++ showcond cc) [lhs, rhs, val]]
    where
    condcodes = [SETNE, SETEQ, SETUGT, SETUGE, SETULT, SETULE]

    selectpats cc ins@[l, r, _, _] = [Selectcc l'' r'' t f cc]
        where
        exts = ext_largest ZExt $ map (mop_width . op_op) [l, r]
        [l', r', t, f] = map (SDOp . fmap to_sdty) ins
        [l'', r''] = zipWith ($) exts [l', r']

showcond SETUGT = "gt"
showcond SETUGE = "ge"
showcond SETULT = "lt"
showcond SETULE = "le"
showcond SETEQ = "eq"
showcond SETNE = "ne"

select_branch_table =
    ["case PRU::" ++ def_name selcc ++ ": return PRU::" ++ qb ++ ";"
        | (selcc, qb) <- zip selectccs corresponding_qb]

reversed_branches =
    ["case PRU::" ++ n ++ ": return PRU::" ++ n' ++ ";"
        | cc <- condcodes, (lhs, rhs) <- r_op 255,
        let n = mangled ("pru_qb" ++ showcond cc) [lhs, rhs],
        let n' = mangled ("pru_qb" ++ showcond (reverse_cc cc)) [lhs, rhs]]
    where condcodes = [SETNE, SETEQ, SETUGT, SETUGE, SETULT, SETULE]

reverse_cc SETUGT = SETULE
reverse_cc SETUGE = SETULT
reverse_cc SETULT = SETUGE
reverse_cc SETULE = SETUGT
reverse_cc SETEQ = SETNE
reverse_cc SETNE = SETEQ
reverse_cc SETGT = SETLE
reverse_cc SETGE = SETLT
reverse_cc SETLT = SETGE
reverse_cc SETLE = SETGT

all_insts = pru_add ++ pru_sub ++ pru_rsb ++ pru_lsl ++ pru_lsr ++ pru_and ++
    pru_or ++ pru_xor ++ pru_not ++ pru_min ++ pru_max ++ pru_nop ++ pru_mov ++
    pru_jmp ++ pru_qbgt ++ pru_qbge ++ pru_qblt ++ pru_qble ++ pru_qbeq ++
    pru_qbne ++ pru_qba ++ pru_qbbs ++ pru_qbbc ++ pru_wbs ++ pru_wbc ++
    pru_loop ++ pru_iloop ++ selectccs

main = do
    args <- getArgs
    case args of
        [which, outfile] ->
            let output | outfile == "-" = putStrLn
                       | otherwise = writeFile outfile in case which of
            "decls" -> output . unlines $ map decl all_insts
            "pats" -> output . unlines $ map (unlines . pat_decls) all_insts
            "selectcc" -> output $ unlines select_branch_table
            "reverse" -> output $ unlines reversed_branches
            _ -> usage
        _ -> usage
    where usage = putStrLn "gen2 (decls|pats|selectcc|reverse) output_file"
