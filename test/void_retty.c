// RUN: %clang -O3 -S -o - -target pru %s | FileCheck %s
// RUN: %clang -O0 -S -o /dev/null -target pru %s

void nothing() {
    // CHECK-LABEL: nothing:
    // CHECK: JMP r3.w2
}
