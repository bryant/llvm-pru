def : Pat<(i8 (trunc reg16: $src)),
          (EXTRACT_SUBREG reg16: $src, sub_8_0)>;

def : Pat<(i8 (trunc (srl reg16: $src, (i32 8)))),
          (EXTRACT_SUBREG reg16: $src, sub_8_8)>;

def : Pat<(i8 (trunc reg32: $src)),
          (EXTRACT_SUBREG reg32: $src, sub_8_0)>;

def : Pat<(i8 (trunc (srl reg32: $src, (i32 8)))),
          (EXTRACT_SUBREG reg32: $src, sub_8_8)>;

def : Pat<(i8 (trunc (srl reg32: $src, (i32 16)))),
          (EXTRACT_SUBREG reg32: $src, sub_8_16)>;

def : Pat<(i8 (trunc (srl reg32: $src, (i32 24)))),
          (EXTRACT_SUBREG reg32: $src, sub_8_24)>;

def : Pat<(i16 (trunc reg32: $src)),
          (EXTRACT_SUBREG reg32: $src, sub_16_0)>;

def : Pat<(i16 (trunc (srl reg32: $src, (i32 16)))),
          (EXTRACT_SUBREG reg32: $src, sub_16_16)>;

def : Pat<(i32 (anyext i16: $p0)),
          (SUBREG_TO_REG (i32 0), reg16: $p0, sub_16_0)>;

def : Pat<(i16 (anyext i8: $p0)),
          (SUBREG_TO_REG (i16 0), reg8: $p0, sub_8_0)>;

def : Pat<(i32 (anyext i8: $p0)),
          (SUBREG_TO_REG (i32 0), reg8: $p0, sub_8_0)>;

// this pattern is useful because the cost of anding with a 16-bit is: ldi, and.
def : Pat<(and reg32: $src, 0xffff),
          (COPY_TO_REGCLASS (i16 (EXTRACT_SUBREG reg32: $src, sub_16_0)),
                            reg32)>;

def : Pat<
    (i16 (build_pair i8: $low, i8: $high)),
    (INSERT_SUBREG (INSERT_SUBREG (i16 (IMPLICIT_DEF)), reg8: $low, sub_8_0),
                reg8: $high, sub_8_8)
>;

def : Pat<
    (i32 (build_pair i16: $low, i16: $high)),
    (INSERT_SUBREG (INSERT_SUBREG (i32 (IMPLICIT_DEF)), reg16: $low, sub_16_0),
                   reg16: $high, sub_16_16)
>;

def : Pat<(targetconst tglobaladdr: $addr),
          (pru_ldi32 tglobaladdr: $addr)>;
def : Pat<(targetconst texternalsym: $addr),
          (pru_ldi32 texternalsym: $addr)>;
def : Pat<(targetconst tblockaddress: $addr),
          (pru_ldi32 tblockaddress: $addr)>;

def : Pat<(pru_call tglobaladdr: $calladdr), (call tglobaladdr: $calladdr)>;
def : Pat<(pru_call texternalsym: $calladdr), (call texternalsym: $calladdr)>;
def : Pat<(pru_call reg32: $calladdr), (call reg32: $calladdr)>;

def : Pat<(add reg32: $src, i32imm_neg_within_255: $imm),
          (pru_sub_reg32_reg32_i32imm reg32: $src, i32imm_neg_within_255: $imm)>;
def : Pat<(add reg16: $src, i16imm_neg_within_255: $imm),
          (pru_sub_reg16_reg16_i16imm reg16: $src, i16imm_neg_within_255: $imm)>;
def : Pat<(add reg8: $src, i8imm_neg_within_255: $imm),
          (pru_sub_reg8_reg8_i8imm reg8: $src, i8imm_neg_within_255: $imm)>;

def : Pat<(sub reg32: $src, i32imm_neg_within_255: $imm),
          (pru_add_reg32_reg32_i32imm reg32: $src, i32imm_neg_within_255: $imm)>;
def : Pat<(sub reg16: $src, i16imm_neg_within_255: $imm),
          (pru_add_reg16_reg16_i16imm reg16: $src, i16imm_neg_within_255: $imm)>;
def : Pat<(sub reg8: $src, i8imm_neg_within_255: $imm),
          (pru_add_reg8_reg8_i8imm reg8: $src, i8imm_neg_within_255: $imm)>;

// TODO: matching on zext-ed result is useful for all non-overflowing binops
def : Pat<(i32 (zext (xor reg8: $src, i8imm_255: $imm))),
          (pru_xor_reg32_reg8_i8imm reg8: $src, i8imm_255: $imm)>;
def : Pat<(i16 (zext (xor reg8: $src, i8imm_255: $imm))),
          (pru_xor_reg16_reg8_i8imm reg8: $src, i8imm_255: $imm)>;
