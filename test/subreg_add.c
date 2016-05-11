// RUN: %clang -O3 -S -o - -target pru %s | FileCheck %s
// RUN: %clang -O0 -S -o /dev/null -target pru %s

// currently fails until we can teach llvm about subregister adds

unsigned int add_halves(unsigned int n) {
    // CHECK-LABEL: add_halves:
    // CHECK-DAG: ADD r14.w2, r14.w2, 1
    // CHECK-DAG: ADD r14.w0, r14.w0, 1
    // CHECK: JMP r3.w2
    unsigned short low = 1 + (unsigned short) n;
    unsigned short high = 1 + (unsigned short) (n >> 16);
    return ((unsigned int) high << 16) | low;
}

unsigned short add_halves2(unsigned short n) {
    // CHECK-LABEL: add_halves2:
    // CHECK-DAG: ADD [[REG:r[0-9]+]].b0, r14.b0, 1
    // CHECK-DAG: ADD [[REG]].b1, r14.b1, 1
    // CHECK: MOV r14, [[REG]].w0
    // CHECK-NEXT: JMP r3.w2
    unsigned char low = 1 + (unsigned char) n;
    unsigned char high = 1 + (unsigned char) (n >> 8);
    return ((unsigned short) high << 8) | low;
}
