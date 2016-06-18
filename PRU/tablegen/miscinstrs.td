let isMoveImm = 1, isReMaterializable = 1 in {
def pru_ldi_i32imm : Instruction {
    let InOperandList = (ins i32imm: $imm);
    let OutOperandList = (outs reg32: $dest);
    let AsmString = "LDI $dest, $imm";
    let Pattern = [(set i32: $dest, i32imm_65535: $imm)];
}

def pru_ldi_i16imm : Instruction {
    let InOperandList = (ins i16imm: $imm);
    let OutOperandList = (outs reg16: $dest);
    let AsmString = "LDI $dest, $imm";
    let Pattern = [(set i16: $dest, i16imm_65535: $imm)];
}

def pru_ldi_i8imm : Instruction {
    let InOperandList = (ins i8imm: $imm);
    let OutOperandList = (outs reg8: $dest);
    let AsmString = "LDI $dest, $imm";
    let Pattern = [(set i8: $dest, i8imm_255: $imm)];
}

// TODO: expand this pseudo into `ldi r.w0; ldi r.w2`
let isPseudo = 1 in {

def pru_ldi32 : Instruction {
    let InOperandList = (ins i32imm: $imm);
    let OutOperandList = (outs reg32: $dest);
    let AsmString = "LDI32 $dest, $imm";
    let Pattern = [(set i32: $dest, imm: $imm)];
}

}
}

def sbbo_multiple : Instruction {
    let InOperandList = (ins addr: $memloc, i8imm: $bytes, reglist: $regs,
                             variable_ops);
    let OutOperandList = (outs);
    let AsmString = "SBBO &$regs, $memloc, $bytes";
    let Pattern = [];
}

def lbbo_multiple : Instruction {
    let InOperandList = (ins addr: $memloc, i8imm: $bytes, reglist: $regs,
                             variable_ops);
    let OutOperandList = (outs);
    let AsmString = "LBBO &$regs, $memloc, $bytes";
    let Pattern = [];
}

def ret : Instruction {
    let InOperandList = (ins);
    let OutOperandList = (outs);
    let AsmString = "JMP r3.w2";
    let Pattern = [(retflag)];
    let isReturn = 1;
    let isTerminator = 1;
    let isBarrier = 1;
}

def addcallstack : Instruction {
    let InOperandList = (ins i32imm: $stacksize);
    let OutOperandList = (outs);
    let AsmString = "addcallstack $stacksize";
    let Pattern = [(SDNode<"ISD::CALLSEQ_START",
                           SDCallSeqStart<[SDTCisVT<0, iPTR>]>,
                           [SDNPHasChain, SDNPOutGlue]>
                    timm: $stacksize)];
    let isPseudo = 1;
}

def subcallstack : Instruction {
    let InOperandList = (ins i32imm: $stacksize, i32imm: $idk);
    let OutOperandList = (outs);
    let AsmString = "subcallstack $stacksize, $idk";
    let Pattern = [(SDNode<"ISD::CALLSEQ_END",
                           SDCallSeqEnd<[SDTCisVT<0, iPTR>, SDTCisVT<1, iPTR>]>,
                           [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue]>
                    timm: $stacksize, timm: $idk)];
    let isPseudo = 1;
}
