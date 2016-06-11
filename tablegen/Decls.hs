{-# LANGUAGE NoMonomorphismRestriction #-}

module Decls where

import Data.Char (toUpper)
import SDNode
import MachNode

type_suffix :: [Operand] -> String
type_suffix = concatMap (('_' :) . mach_show)

reg_suffix = type_suffix . map Reg

pru3 llname opcode attrs l r = MachInstr instr [m l, m r]
    where
    instr = Instruction (Just Unknown) [Unknown, Unknown] asmp llname attrs
    asmp [l, r] d = opcode ++ " " ++ comma_join (map opshow [d, l, r])

pru2 llname opcode attrs src = MachInstr instr [m src]
    where
    instr = Instruction (Just Unknown) [Unknown] asmp llname attrs
    asmp [src] dest = opcode ++ " " ++ comma_join (map opshow [dest, src])

pru_branch :: (ToMLeaf u, ToMLeaf v, ToMLeaf w) => String -> String -> [IAttr]
           -> u -> v -> w -> MachNode
pru_branch llname opcode attrs bb l r = MachInstr instr [m bb, m l, m r]
    where
    instr = Instruction Nothing [BasicBlock, Unknown, Unknown] asmp llname attrs
    asmp [bb', l, r] _ = opcode ++ " " ++ comma_join (map opshow [bb', l, r])

pru_select llname attrs l r t f = MachInstr instr [m l, m r, m t, m f]
    where
    instr = Instruction (Just Unknown) (replicate 4 Unknown) asmp llname attrs
    asmp inops dest = llname ++ " " ++ comma_join (map opshow $ dest : inops)

{-
* add r.r.op255
* sub
* rsb
* and
* or
* xor
* min
* max
* nop
* lsl r.r.op31
* lsr
* not r.r
* mov
ldi r.i65535
ldi32 r.i4294967295
* lbbo r.r.op124
* sbbo
lbco r.c.op124
sbco
xin i253.r.i124
xout
sxin
sxout
sxchg
* jmp jt
* qbgt jt.r.op255
* qbge
* qblt
* qble
* qbeq
* qbne
* qba
* qbbs jt.r.op31
* qbbc
wbs
wbc
hlt -
slp
loop jt.r
iloop
selectcc op.op.op.op.cc
-}

-- r-r-r forms
[pru_add, pru_sub, pru_rsb, pru_and, pru_or, pru_xor, pru_min, pru_max, pru_nop,
    pru_lsl, pru_lsr] =
        [pru3 pref mnem [] |
            op <- words "add sub rsb and or xor min max nop lsl lsr",
            let pref = "pru_" ++ op,
            let mnem = map toUpper op]

-- r-r-imm forms. note: rematerializable
[pru_add_imm, pru_sub_imm, pru_rsb_imm, pru_and_imm, pru_or_imm, pru_xor_imm,
    pru_min_imm, pru_max_imm, pru_nop_imm, pru_lsl_imm, pru_lsr_imm] =
        [pru3 pref mnem [IsRematerializable True] |
            op <- words "add sub rsb and or xor min max nop lsl lsr",
            let pref = "pru_" ++ op ++ "_imm",
            let mnem = map toUpper op]

-- branches
[pru_qbgt, pru_qbge, pru_qblt, pru_qble, pru_qbeq, pru_qbne, pru_qba, pru_qbbs,
    pru_qbbc] =
        [pru_branch pref mnem [IsBranch True, IsTerminator True] |
            op <- words "gt ge lt le eq ne a bs bc",
            let pref = "pru_qb" ++ op,
            let mnem = map toUpper $ "qb" ++ op]

pru_mov = pru2 "pru_mov" "MOV" []

pru_jmp dest = MachInstr instr [m dest]
    where
    asmp [dest] _ = "JMP " ++ opshow dest
    attrs = [IsBranch True, IsTerminator True, IsBarrier True]
    instr = Instruction Nothing [BasicBlock] asmp "pru_jmp" attrs

pru_call addr = MachInstr instr [m addr]
    where
    instr = Instruction Nothing [Unknown] asmp "pru_call" attrs
    attrs = [IsCall True, Defs ["r3_w2"], Uses ["r2"]]
    asmp [calladdr] _ = "JAL r3.w2, " ++ opshow calladdr

-- load/store
pru_lbbo width addr = MachInstr instr [m addr]
    where
    instr = Instruction (Just $ Reg width) [Addr] asmp llname []
    asmp [memloc] dest =
        "LBBO &" ++ comma_join [opshow dest, opshow memloc, show bytes]
    llname = "pru_lbbo" ++ reg_suffix [width]
    bytes = width `quot` 8

pru_sbbo width val addr = MachInstr instr [m val, m addr]
    where
    instr = Instruction Nothing [Reg width, Addr] asmp llname []
    asmp [reg, memloc] _ =
        "SBBO &" ++ comma_join [opshow reg, opshow memloc, show bytes]
    llname = "pru_sbbo" ++ reg_suffix [width]
    bytes = width `quot` 8

[pru_selectne, pru_selecteq, pru_selectgt, pru_selectge, pru_selectlt,
    pru_selectle] =
        [pru_select ("pru_select" ++ cmp) [UsesCustomInserter True] |
            cmp <- words "ne eq gt ge lt le"]
