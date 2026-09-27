// AArch64 assembly example: strlen and a small SIMD add.
        .arch   armv8-a
        .text
        .global strlen
        .type   strlen, %function
strlen:
        mov     x1, x0
.Lloop:
        ldrb    w2, [x1], #1
        cbnz    w2, .Lloop
        sub     x0, x1, x0
        sub     x0, x0, #1
        ret
        .size   strlen, .-strlen

        .global vec_add
vec_add:
        ld1     {v0.4s}, [x0]
        ld1     {v1.4s}, [x1]
        fadd    v2.4s, v0.4s, v1.4s
        st1     {v2.4s}, [x2]
        ret

        .global cond_example
cond_example:
        cmp     x0, #0
        csel    x0, x1, x2, eq
        b.lt    .Lneg
        ret
.Lneg:
        neg     x0, x0
        ret
