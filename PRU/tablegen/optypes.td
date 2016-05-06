def i8imm_31 : ImmLeaf<i8, [{return (uint8_t) Imm <= 31;}]>;
def i16imm_31 : ImmLeaf<i16, [{return (uint16_t) Imm <= 31;}]>;
def i32imm_31 : ImmLeaf<i32, [{return (uint32_t) Imm <= 31;}]>;

def i8imm_255 : ImmLeaf<i8, [{return (uint8_t) Imm <= 255;}]>;
def i16imm_255 : ImmLeaf<i16, [{return (uint16_t) Imm <= 255;}]>;
def i32imm_255 : ImmLeaf<i32, [{return (uint32_t) Imm <= 255;}]>;

def i16imm_65535 : ImmLeaf<i16, [{return (uint16_t) Imm <= 65535;}]>;
def i32imm_65535 : ImmLeaf<i32, [{return (uint32_t) Imm <= 65535;}]>;

def jump_target : Operand<OtherVT> { let OperandType = "OPERAND_PCREL"; }

def gaddr : Operand<iPTR>;

def addr : Operand<iPTR>, ComplexPattern<iPTR, 2, "SelectAddress"> {
    let MIOperandInfo = (ops reg32: $base, i32imm: $offset);
    let PrintMethod = "print_addr";
}

def regaddr : Operand<iPTR>, ComplexPattern<iPTR, 2, "SelectAddressReg"> {
    let MIOperandInfo = (ops reg32: $base, reg32: $offset);
    let PrintMethod = "print_addr";
}

def reglist : Operand<i32>, AsmOperandClass {
    let Name = "reglistop";
}

def build_pair : SDNode<"ISD::BUILD_PAIR",
    SDTypeProfile<1, 2, [SDTCisInt<0>, SDTCisInt<1>, SDTCisSameAs<1, 2>]>>;

def retflag : SDNode<"PRUISD::RET_FLAG", SDTNone,
                     [SDNPHasChain, SDNPOptInGlue, SDNPVariadic]>;

def sdt_call : SDTypeProfile<0, -1, [SDTCisPtrTy<0>]>;

def pru_call : SDNode<"PRUISD::CALL",
                      sdt_call,
                      [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue, SDNPVariadic]>;

def targetconst : SDNode<"PRUISD::TargetConst",
                         SDTypeProfile<1, 1, [SDTCisPtrTy<0>, SDTCisPtrTy<1>]>>;
