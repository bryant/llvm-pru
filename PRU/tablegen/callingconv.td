def callee_saved : CalleeSavedRegs<(add r3_w2, (sequence "r%d", 4, 13))>;

def i8_args : CalleeSavedRegs<
    (interleave (sequence "r%d_b0", 14, 29), (sequence "r%d_b1", 14, 29),
                (sequence "r%d_b2", 14, 29), (sequence "r%d_b3", 14, 29))
>;

def i16_args : CalleeSavedRegs<
    (interleave (sequence "r%d_w0", 14, 29), (sequence "r%d_w1", 14, 29),
                (sequence "r%d_w2", 14, 29))
>;

def i32_args : CalleeSavedRegs<(sequence "r%d", 14, 29)>;

def all_reg8 : CalleeSavedRegs<
    (interleave (sequence "r%d_b0", 0, 31), (sequence "r%d_b1", 0, 31),
                (sequence "r%d_b2", 0, 31), (sequence "r%d_b3", 0, 31))
>;

def all_reg16: CalleeSavedRegs<
    (interleave (sequence "r%d_w0", 0, 31), (sequence "r%d_w1", 0, 31),
                (sequence "r%d_w2", 0, 31))
>;

def all_reg32: CalleeSavedRegs<(sequence "r%d", 0, 31)>;
