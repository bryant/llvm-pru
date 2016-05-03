// RUN: %clang -O3 -S -o - -target pru %s | FileCheck %s

// currently fails until we are able to transform:
// a:i32 `binop_32_32` i32 (b:i32 & 0xffff) =>
//      a:i32 `binop_32_16` extract_subreg b:i32 sub_16_0
// a:i32 `binop_32_32` i32 (b:i32 & 0xffff0000) =>
//      a:i32 `binop_32_16` extract_subreg b:i32 sub_16_16

typedef union {
    unsigned int i;
    unsigned short s;
    unsigned char c;
} U;

unsigned low_order_extract(U a, U b) {
    // CHECK-LABEL: low_order_extract:
    // CHECK: ADD r14, r14.w0, r15.b0
    return a.s + b.c;
}

typedef union {
    unsigned short s;
    unsigned char c;
} V;

unsigned low_order_extract2(V a, V b) {
    // CHECK-LABEL: low_order_extract2:
    // CHECK: ADD r14, r14.b0, r15.b0
    return a.c + b.c;
}
