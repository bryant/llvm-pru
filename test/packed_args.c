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
