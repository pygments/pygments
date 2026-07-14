package main

func main() {
	var (
		float float32
		_ = float64(float)
		_ map[string]any
		_ chan int
		_ = [...]int{0o755, 0O_644, 0777}
		x = 0b1100 ^ 0B1001 | 0b00_10
		_ = 1_000_000
		_ = 0x_fefe_1a3d
		_ = !(x > 0 && x < 8)
	)
	x ^= 0b_0101
}
