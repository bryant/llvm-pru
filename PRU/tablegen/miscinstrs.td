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
    let Pattern = [(set i16: $dest, imm: $imm)];
}

def pru_ldi_i8imm : Instruction {
    let InOperandList = (ins i8imm: $imm);
    let OutOperandList = (outs reg8: $dest);
    let AsmString = "LDI $dest, $imm";
    let Pattern = [(set i8: $dest, imm: $imm)];
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

def sbbo_r32 : Instruction {
    let InOperandList = (ins reg32: $r, addr: $memloc);
    let OutOperandList = (outs);
    let AsmString = "SBBO &$r, $memloc, 4";
    let Pattern = [(store i32: $r, addr: $memloc)];
}

def sbbo_r16 : Instruction {
    let InOperandList = (ins reg16: $r, addr: $memloc);
    let OutOperandList = (outs);
    let AsmString = "SBBO &$r, $memloc, 2";
    let Pattern = [(store i16: $r, addr: $memloc)];
}

def sbbo_r8 : Instruction {
    let InOperandList = (ins reg8: $r, addr: $memloc);
    let OutOperandList = (outs);
    let AsmString = "SBBO &$r, $memloc, 1";
    let Pattern = [(store i8: $r, addr: $memloc)];
}

def sbbo_multiple : Instruction {
    let InOperandList = (ins addr: $memloc, i8imm: $bytes, reglist: $regs,
                             variable_ops);
    let OutOperandList = (outs);
    let AsmString = "SBBO &$regs, $memloc, $bytes";
    let Pattern = [];
}

let isReMaterializable = 1 in {
def lbbo_r32 : Instruction {
    let InOperandList = (ins addr: $memloc);
    let OutOperandList = (outs reg32: $r);
    let AsmString = "LBBO &$r, $memloc, 4";
    let Pattern = [(set i32: $r, (load addr: $memloc))];
}

def lbbo_r16 : Instruction {
    let InOperandList = (ins addr: $memloc);
    let OutOperandList = (outs reg16: $r);
    let AsmString = "LBBO &$r, $memloc, 2";
    let Pattern = [(set i16: $r, (load addr: $memloc))];
}

def lbbo_r8 : Instruction {
    let InOperandList = (ins addr: $memloc);
    let OutOperandList = (outs reg8: $r);
    let AsmString = "LBBO &$r, $memloc, 1";
    let Pattern = [(set i8: $r, (load addr: $memloc))];
}
}

def lbbo_multiple : Instruction {
    let InOperandList = (ins addr: $memloc, i8imm: $bytes, reglist: $regs,
                             variable_ops);
    let OutOperandList = (outs);
    let AsmString = "LBBO &$regs, $memloc, $bytes";
    let Pattern = [];
}

def sbbo_r32_r : Instruction {
    let InOperandList = (ins reg32: $r, regaddr: $memloc);
    let OutOperandList = (outs);
    let AsmString = "SBBO &$r, $memloc, 4";
    let Pattern = [(store i32: $r, regaddr: $memloc)];
}

def sbbo_r16_r : Instruction {
    let InOperandList = (ins reg16: $r, regaddr: $memloc);
    let OutOperandList = (outs);
    let AsmString = "SBBO &$r, $memloc, 2";
    let Pattern = [(store i16: $r, regaddr: $memloc)];
}

def sbbo_r8_r : Instruction {
    let InOperandList = (ins reg8: $r, regaddr: $memloc);
    let OutOperandList = (outs);
    let AsmString = "SBBO &$r, $memloc, 1";
    let Pattern = [(store i8: $r, regaddr: $memloc)];
}

def lbbo_r32_r : Instruction {
    let InOperandList = (ins regaddr: $memloc);
    let OutOperandList = (outs reg32: $r);
    let AsmString = "LBBO &$r, $memloc, 4";
    let Pattern = [(set i32: $r, (load regaddr: $memloc))];
}

def lbbo_r16_r : Instruction {
    let InOperandList = (ins regaddr: $memloc);
    let OutOperandList = (outs reg16: $r);
    let AsmString = "LBBO &$r, $memloc, 2";
    let Pattern = [(set i16: $r, (load regaddr: $memloc))];
}

def lbbo_r8_r : Instruction {
    let InOperandList = (ins regaddr: $memloc);
    let OutOperandList = (outs reg8: $r);
    let AsmString = "LBBO &$r, $memloc, 1";
    let Pattern = [(set i8: $r, (load regaddr: $memloc))];
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

def call : Instruction {
    let InOperandList = (ins gaddr: $calladdr);
    let OutOperandList = (outs);
    let AsmString = "JAL r3.w2, $calladdr";
    let Pattern = [];
    let Uses = [r2];  // protects stack adj from deletion
    let Defs = [r3_w2];
    let isCall = 1;
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
