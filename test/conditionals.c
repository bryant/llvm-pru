// RUN: %clang -O3 -S -o - -target pru -mllvm -bbo-combiner2 %s | FileCheck %s
// RUN: %clang -O0 -S -o /dev/null -target pru %s

unsigned cond_test_simple(unsigned char selector) {
    unsigned rv;
    // CHECK-LABEL: cond_test_simple:
    // CHECK: QBEQ {{[^,]+}}, r14.b0, 0x1
    if (selector == 1) {
        rv = 4;
    } else {
        rv = 29;
    }
    return rv;
}

unsigned asym_reg_sizes(unsigned char lhs, unsigned rhs) {
    // CHECK-LABEL: asym_reg_sizes:
    // CHECK-NOT: MOV
    // CHECK-NOT: AND
    if (lhs >= rhs) {
        return 42;
    } else {
        return 24;
    }
}

// TODO: handle signed cmps
