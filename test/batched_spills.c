// RUN: %clang -O3 -S -o - -target pru -fverbose-asm -mllvm -bbo-combiner2 %s \
// RUN:     | FileCheck %s

#pragma pack(1)

typedef struct {
    unsigned char p0;
    unsigned int pint[256];
} VeryLarge;

VeryLarge batched_spill_slots(
    unsigned p0, unsigned p1, unsigned p2, unsigned p3, unsigned p4,
    unsigned p5, unsigned p6, unsigned p7, unsigned p8, unsigned p9,
    unsigned p10, unsigned p11, unsigned p12, unsigned p13, unsigned p14,
    unsigned p15, unsigned p16, unsigned p17, unsigned p18, unsigned p19,
    unsigned p20, unsigned p21, unsigned p22, unsigned p23, unsigned p24,
    unsigned p25, unsigned p26, unsigned p27, unsigned p28, unsigned p29,
    unsigned p30, unsigned p31, unsigned p32, unsigned int p33,
    unsigned int p34, unsigned int p35, unsigned int p36, unsigned int p37,
    unsigned int p38, unsigned int p39, unsigned int p40, unsigned int p41,
    unsigned int p42, unsigned int p43, unsigned int p44, unsigned int p45,
    unsigned int p46, unsigned int p47, unsigned int p48, unsigned int p49,
    unsigned int p50, unsigned int p51, unsigned int p52, unsigned int p53,
    unsigned int p54, unsigned int p55, unsigned int p56, unsigned int p57,
    unsigned int p58, unsigned int p59, unsigned int p60, VeryLarge s) {
    // CHECK-LABEL: batched_spill_slots:
    // CHECK: SUB r2, r2, {{.+}}
    // CHECK: SBBO &r3.w2, r2, {{[^,]+}}, {{.+}} ; batched:

    unsigned int i;
    for (i = 0; i < 256; ++i) {
        s.pint[i] += 25;
    }

    s.pint[0] += p0;
    s.pint[1] += p1;
    s.pint[2] += p2;
    s.pint[3] += p3;
    s.pint[4] += p4;
    s.pint[5] += p5;
    s.pint[6] += p6;
    s.pint[7] += p7;
    s.pint[8] += p8;
    s.pint[9] += p9;
    s.pint[10] += p10;
    s.pint[11] += p11;
    s.pint[12] += p12;
    s.pint[13] += p13;
    s.pint[14] += p14;
    s.pint[15] += p15;
    s.pint[16] += p16;
    s.pint[17] += p17;
    s.pint[18] += p18;
    s.pint[19] += p19;
    s.pint[20] += p20;
    s.pint[21] += p21;
    s.pint[22] += p22;
    s.pint[23] += p23;
    s.pint[24] += p24;
    s.pint[25] += p25;
    s.pint[26] += p26;
    s.pint[27] += p27;
    s.pint[28] += p28;
    s.pint[29] += p29;
    s.pint[30] += p30;
    s.pint[31] += p31;
    s.pint[32] += p32;
    s.pint[33] += p33;
    s.pint[34] += p34;
    s.pint[35] += p35;
    s.pint[36] += p36;
    s.pint[37] += p37;
    s.pint[38] += p38;
    s.pint[39] += p39;
    s.pint[40] += p40;
    s.pint[41] += p41;
    s.pint[42] += p42;
    s.pint[43] += p43;
    s.pint[44] += p44;
    s.pint[45] += p45;
    s.pint[46] += p46;
    s.pint[47] += p47;
    s.pint[48] += p48;
    s.pint[49] += p49;
    s.pint[50] += p50;
    s.pint[51] += p51;
    s.pint[52] += p52;
    s.pint[53] += p53;
    s.pint[54] += p54;
    s.pint[55] += p55;
    s.pint[56] += p56;
    s.pint[57] += p57;
    s.pint[58] += p58;
    s.pint[59] += p59;
    s.pint[60] += p60;

    if (p32) {
        s.pint[32] += p32;
    }
    // CHECK: LBBO &r3.w2, r2, {{[^,]+}}, {{.+}} ; batched:
    // CHECK-NEXT: ADD r2, r2, {{.+}}
    // CHECK-NEXT: JMP r3.w2
    return s;
}
