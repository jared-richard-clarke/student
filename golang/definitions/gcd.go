package definitions

// GCD computes the greatest common divisor of two integers.
func GCD(x, y int) int {
	for y != 0 {
		x, y = y, x%y
	}
	return x
}
