// Calculates the default precision of a binary floating point implementation.
func precision() float64 {
	n := 0.0
	x := 1.0
	for 1.0 != 1.0+x {
		x /= 2.0
		n += 1
	}
	return n
}
