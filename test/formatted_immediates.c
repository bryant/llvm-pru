// RUN: %clang -O3 -S -o - -target pru %s | FileCheck %s
// RUN: %clang -O0 -S -o /dev/null -target pru %s

// check that immediate operands are printed correctly, e.g., "0x80" instead of
// "0xffffff80".

unsigned return32() {
    // CHECK-LABEL: return32:
    // CHECK-NEXT: LDI32 r14, 0xdeadbeef
    // CHECK-NEXT: JMP r3.w2
    return 0xdeadbeef;
}

unsigned short return16() {
    // CHECK-LABEL: return16:
    // CHECK-NEXT: LDI r14, 0x8000
    // CHECK-NEXT: JMP r3.w2
    return 0x8000;
}

unsigned char return8() {
    // CHECK-LABEL: return8:
    // CHECK-NEXT: LDI r14, 0x80
    // CHECK-NEXT: JMP r3.w2
    return 0x80;
}
