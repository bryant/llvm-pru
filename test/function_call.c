// RUN: %clang -O3 -S -o - -target pru -fverbose-asm -mllvm -bbo-combiner2 %s \
// RUN:     | FileCheck %s

unsigned int weird_jump(unsigned int (*jump_point)()) {
    // CHECK-LABEL: weird_jump:
    // CHECK: JAL r3.w2, r14
    return 3 + (*jump_point)();
}

// note that call addresses are technically sixteen-bit instruction sequence
// counters, so below is technically allowed (in fact, clpru even permits `JAL`
// to be paired with eight-bit registers).
unsigned int call_reg16(unsigned short instr_addr) {
    // CHECK-LABEL: call_reg16:
    return 3 + ((unsigned int (*)()) instr_addr)();
}

unsigned call_printf() {
    // CHECK-LABEL: call_printf
    // CHECK: SBBO &r3.w2, r2, 0, 0x2
    // CHECK: JAL r3.w2, {{.+}}
    printf("printf called\n");
    return 0;
}
