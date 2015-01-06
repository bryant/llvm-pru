def : Pat<(i8 (trunc reg16: $src)),
          (EXTRACT_SUBREG reg16: $src, sub_8_0)>;

def : Pat<(i8 (trunc reg16: $src)),
          (EXTRACT_SUBREG reg16: $src, sub_8_8)>;

def : Pat<(i8 (trunc reg32: $src)),
          (EXTRACT_SUBREG reg32: $src, sub_8_0)>;

def : Pat<(i16 (trunc reg32: $src)),
          (EXTRACT_SUBREG reg32: $src, sub_16_0)>;

def : Pat<(i16 (trunc reg32: $src)),
          (EXTRACT_SUBREG reg32: $src, sub_16_16)>;
