// RUN: %clang -O3 -S -o - -target pru %s
// RUN: %clang -O0 -S -o /dev/null -target pru %s

unsigned char subtract(unsigned char a) { return a - 128; }

unsigned char subtract2(unsigned char a) { return a - 127; }
