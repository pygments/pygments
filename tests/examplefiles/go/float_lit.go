package main

import "fmt"

func main() {
	_ = []float64{
		0.,
		0_0.,
		0.0,
		0.0_0,
		0.e0,
		0.e0,
		0.e+0,
		0.e-0,
		0.0e0,
		0.0_0e0,
		0.0e0_0,

		0e0,
		0_0e0,

		.0,
		.0_0,
		.0e0,
		.0e0_0,

		0xfp0,
		0xfp0,
		0xfp+0,
		0xfp-0,
		0xfp0_0,
		0xf_fp0,
		0x_fp0,
		0xf.p0,
		0xf_f.p0,
		0x_f.p0,
		0xf.fp0,
		0xf.f_fp0,

		0x.fp0,
		0x.f_fp0,

		1234567890.1234567890e123,
		0e4567890,
		0xabcdef.abcdefp0123,
		0x0123456789.0123456789p456,
		0xABCDEF.ABCDEFp789,
		0x_fa0_1E.D2_bbp-3_0_1,
	}
	fmt.Println("compiles")
}
