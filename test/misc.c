// RUN: %clang -O3 -S -o - -target pru -mllvm -bbo-combiner2 %s | FileCheck %s

#pragma pack(1)

typedef struct {
    unsigned char a;
    unsigned short b;
    unsigned char c;
} Sized32;

Sized32 ti_buggy_codegen(Sized32 a) {
    // ti a) generates inefficient stores to b) assumed-free address 0x00 for
    // the retval.
    // -O4:
    // 00000000                   inc_sized64:
    // 00000000     240000e0      LDI R0, 0
    // 00000004     01010e01      ADD R1.b0, R14.b0, 1
    // 00000008     e1000001      SBBO &R1.b0, R0, 0, 1   ; addr 0x00?!
    // 0000000c     0109ae81      ADD R1.w0, R14.w1, 9
    // 00000010     e1010081      SBBO &R1.b0, R0, 1, 2   ; why even store
    // 00000014     f100208e      LBBO &R14.b0, R0, 0, 4
    // 00000018     20c30000      JMP R3.w2

    // CHECK-LABEL: ti_buggy_codegen:
    // CHECK-DAG: ADD r14.b0, r14.b0, 0x1
    // CHECK-DAG: ADD r14.w1, r14.w1, 0x9
    // CHECK-NOT: SBBO
    // CHECK-NOT: LBBO
    Sized32 rv = {a.a + 1, a.b + 9};
    return rv;
}

unsigned there_should_be_no_loop(unsigned n, unsigned char m) {
    // ti bug: triggers insertion of `loop` instr that isn't elided:
    // 00000000                   add:
    // 00000000     51000f03      QBEQ $C$L1, R15.b0, 0
    // 00000004     000feeee      ADD R14, R14, R15.b0
    // 00000008     300f0001      LOOP $C$L1, R15.b0
    // 0000000c                   $C$L2:
    // 0000000c                   $C$L1:
    // 0000000c     20c30000      JMP R3.w2

    // CHECK-LABEL: there_should_be_no_loop:
    // CHECK: ADD r14, {{r14|r15.b0}}, {{r14|r15.b0}}
    // CHECK-NEXT: JMP r3.w2
    unsigned i;
    for (i = 0; i < m; ++i) {
        n += 1;
    }
    return n;
}
