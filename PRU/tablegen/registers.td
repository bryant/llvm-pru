class Reg<string nombre, list<string> alts = []> : Register<nombre, alts> {
    let Namespace = "PRU";
}

// sameness of subregindex instances relied upon by EXTRACT_SUBREG
def sub_8_0 : SubRegIndex<8>;
def sub_8_8 : SubRegIndex<8, 8>;
def sub_16_0 : SubRegIndex<16>;
def sub_16_16 : SubRegIndex<16, 16>;

def sub_8_16 : ComposedSubRegIndex<sub_16_16, sub_8_0>;
def sub_8_24 : ComposedSubRegIndex<sub_16_16, sub_8_8>;

foreach reg = 0-31 in {
    foreach byte = 0-3 in {
        def r # reg # _b # byte : Reg<"r" # reg # ".b" # byte>;
    }

    foreach word = [0, 2] in {
        def r # reg # _w # word : Reg<"r" # reg # ".w" # word> {
            let SubRegs = [!cast<Reg>("r" # reg # "_b" # word),
                           !cast<Reg>("r" # reg # "_b" # !add(word, 1))];
            let SubRegIndices = [sub_8_0, sub_8_8];
            let CoveredBySubRegs = 1;
        }
    }

    def r # reg # _w1 : Reg<"r" # reg # ".w1"> {
        let Aliases = [!cast<Reg>("r" # reg # "_w0"),
                       !cast<Reg>("r" # reg # "_w2")];

        let SubRegs = [!cast<Reg>("r" # reg # "_b1"),
                       !cast<Reg>("r" # reg # "_b2")];
        let SubRegIndices = [sub_8_0, sub_8_8];
        let CoveredBySubRegs = 1;
    }

    def r # reg : Reg<"r" # reg> {
        let SubRegs = [!cast<Reg>("r" # reg # "_w0"),
                       !cast<Reg>("r" # reg # "_w2")];
        let SubRegIndices = [sub_16_0, sub_16_16];
        let CoveredBySubRegs = 1;
    }
}

foreach n = 0-3 in {
    def b # n : Reg<"b" # n> {
        let SubRegs = [!cast<Reg>("r0_b" # n)];
        let SubRegIndices = [sub_8_8];
        let CoveredBySubRegs = 1;
    }
}


def breg : RegisterClass<"PRU", [i8], 8, (sequence "b%u", 0, 3)>;

def reg8 : RegisterClass<"PRU", [i8], 8, (add
    (sequence "r%u_b0", 0, 31),
    (sequence "r%u_b1", 0, 31),
    (sequence "r%u_b2", 0, 31),
    (sequence "r%u_b3", 0, 31)
)>;

def reg16 : RegisterClass<"PRU", [i16], 8, (add
    (sequence "r%u_w0", 0, 31),
    (sequence "r%u_w1", 0, 31),
    (sequence "r%u_w2", 0, 31)
)>;

def reg32 : RegisterClass<"PRU", [i32], 8, (sequence "r%u", 0, 31)>;
