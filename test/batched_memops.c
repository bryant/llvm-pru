// RUN: %clang -O3 -S -o - -target pru %s -mllvm -bbo-combiner2 \
// RUN:     | FileCheck -check-prefix=BATCHED %s
// RUN: %clang -O3 -S -o - -target pru %s | FileCheck -check-prefix=UNBATCHED %s

#pragma pack(1)

typedef struct {
    unsigned char a;
    unsigned short b;
    unsigned int c;
    unsigned char d;
    unsigned short e;
    unsigned int f;
    unsigned char g;
    unsigned short h;
} Sponge;

Sponge inc_sponge(Sponge s) {
    // CHECK-LABEL: inc_sponge:
    // BATCHED: LBBO &{{[^,]+}}, r2, 0, 0xa
    // UNBATCHED: LBBO &{{[^,]+}}, r2, 0, 1
    s.a += 1;
    s.b += 2;
    s.c += 3;
    s.d += 4;
    s.e += 5;
    s.f += 6;
    s.g += 7;
    s.h += 8;

    return s;
}
