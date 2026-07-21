package main

import "fmt"

func main() {
	_ = []complex128{
		0.i,
		0_0.i,
		0.0i,
		0.0_0i,
		0.e0i,
		0.e0i,
		0.e+0i,
		0.e-0i,
		0.0e0i,
		0.0_0e0i,
		0.0e0_0i,

		0e0i,
		0_0e0i,

		.0i,
		.0_0i,
		.0e0i,
		.0e0_0i,

		0xfp0i,
		0xfp0i,
		0xfp+0i,
		0xfp-0i,
		0xfp0_0i,
		0xf_fp0i,
		0x_fp0i,
		0xf.p0i,
		0xf_f.p0i,
		0x_f.p0i,
		0xf.fp0i,
		0xf.f_fp0i,

		0x.fp0i,
		0x.f_fp0i,

		1234567890.1234567890e123i,
		0e4567890i,
		0xabcdef.abcdefp0123i,
		0x0123456789.0123456789p456i,
		0xABCDEF.ABCDEFp789i,
		0x_fa0_1E.D2_bbp-3_0_1i,

		1234i,
		01234i,
		0o1234i,
		0b10101i,
		0xabcdefi,
	}
	fmt.Println("compiles")
}
