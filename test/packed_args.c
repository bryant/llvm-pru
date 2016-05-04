// RUN: %clang -O3 -S -o - -target pru -fverbose-asm -mllvm -bbo-combiner2 %s \
// RUN:     | FileCheck %s

#pragma pack(1)

// we expect `a`, `b`, `c` to be packed into r14.b0, r15, and r14.w1,
// respectively. furthermore, we expect the codegen to be efficient enough to
// subtract directly to the return value register, r14.
unsigned int add_sub(unsigned char a, unsigned int b, unsigned short c) {
    // CHECK-LABEL: add_sub:
    // CHECK-DAG: ADD {{[^,]+}}, r14.b0, r15
    // CHECK-DAG: SUB r14, {{[^,]+}}, r14.w1
    return a + b - c;
}

typedef struct {
    unsigned int a;
    unsigned int b;
} T;

unsigned struct_should_be_in_regs(T t) {
    // CHECK-LABEL: struct_should_be_in_regs:
    // CHECK: ADD r14, r14, r15
    return t.a + t.b;
}

typedef struct {
    unsigned int b;
    unsigned char a;
} S;

unsigned struct_should_be_in_stack(S s) {
    // CHECK-LABEL: struct_should_be_in_stack:
    // CHECK: LBBO &{{[^,]+}}, r2, 0, {{.+}} ; batched
    // CHECK: ADD r14, {{[^,]+}}, {{[^,]+}}
    return s.a + s.b;
}

typedef struct {
    unsigned char a;
    unsigned int b;
    unsigned short c;
    unsigned short d;
} Large;

unsigned larger_than_64_goes_into_stack(Large l) {
    // CHECK-LABEL: larger_than_64_goes_into_stack:
    // CHECK: LBBO &{{[^,]+}}, r2, 0, {{.+}} ; batched
    return l.a + l.b + l.c + l.d;
}

unsigned arg15_fully_in_stack(unsigned arg0, unsigned arg1, unsigned arg2,
                              unsigned arg3, unsigned arg4, unsigned arg5,
                              unsigned arg6, unsigned arg7, unsigned arg8,
                              unsigned arg9, unsigned arg10, unsigned arg11,
                              unsigned arg12, unsigned arg13, unsigned arg14,
                              unsigned long long arg15, unsigned short arg16) {
    // ensure that arg14 and arg16 reside in r28 and r29.w0, respectively. arg15
    // should be on the stack since i64s are either fully in reg or fully on
    // stack otherwise.

    // CHECK-LABEL: arg15_fully_in_stack:
    // CHECK: LBBO &r{{[0-9]+}}, r2, 0, 4
    // CHECK: ADD r{{[0-9]+}}, r{{[0-9]+}}, r{{[0-9]+}}
    // CHECK: SUB r14, r{{[0-9]+}}, r{{[0-9]+}}.w0
    return arg14 + arg15 - arg16;
}
