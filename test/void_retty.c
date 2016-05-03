// RUN: %clang -O3 -S -o - -target pru %s | FileCheck %s

void nothing() {
    // CHECK-LABEL: nothing:
    // CHECK: JMP r3.w2
}
