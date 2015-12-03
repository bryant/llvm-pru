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
