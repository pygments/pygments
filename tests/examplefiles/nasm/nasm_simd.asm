; test source for SIMD operations
; not intended to be executable
bits 64

kmovq k1, [rel mask]
vpxor xmm0, xmm0, xmm0
vmovdqa32 zmm30, [abs data]
vpaddd zmm0{k1}, zmm0, zmm30
vpxor xmm1, xmm1, [rel xmm01]

mask dq 0xAAAAAAAA55555555
data: times 16 dd 0x12345678
xmm01: times 4 dd 0
