// RUN: %clang -O3 -S -o - -target pru %s | FileCheck %s
// RUN: %clang -O0 -S -o /dev/null -target pru %s

unsigned char zextadd(unsigned int a, unsigned short b) {
    // CHECK-LABEL: zextadd:
    // CHECK: ADD [[REG:r[0-9]+]].b0, {{(r14|r15).b0}}, {{(r14|r15).b0}}
    // CHECK-NEXT: MOV r14, [[REG]].b0
    // ^ zero extension from result (char8) to return reg is needed
    return a + b;
}

unsigned short zextadd2(unsigned int a, unsigned char b) { 
    // CHECK-LABEL: zextadd2:
    // CHECK: ADD [[REG:r[0-9]+]].w0, {{(r14|r15.b0)}}, {{(r14|r15.b0)}}
    // CHECK-NEXT: MOV r14, [[REG]].w0
    return a + b;
}

unsigned int zextadd3(unsigned short a, unsigned char b) { 
    // CHECK-LABEL: zextadd3:
    // CHECK: ADD r14, {{r14(.w0|.b2)}}, {{r14(.w0|.b2)}}
    return a + b;
}

unsigned int lsl(unsigned short a, unsigned char b) {
    // CHECK-LABEL: lsl:
    // CHECK: LSL r14, {{r14(.w0|.b2)}}, {{r14(.w0|.b2)}}
    return a << b;
}
