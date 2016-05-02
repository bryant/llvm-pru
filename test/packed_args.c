// RUN: %clang -O3 -S -o - -target pru -mllvm -bbo-combiner2 %s | FileCheck %s

// we expect `a`, `b`, `c` to be packed into r14.b0, r15, and r14.w1,
// respectively. furthermore, we expect the codegen to be efficient enough to
// subtract directly to the return value register, r14.
unsigned int add_sub(unsigned char a, unsigned int b, unsigned short c) {
    // CHECK-LABEL: add_sub:
    // CHECK-DAG: ADD {{[^,]+}}, r14.b0, r15
    // CHECK-DAG: SUB r14, {{[^,]+}}, r14.w1
    return a + b - c;
}
