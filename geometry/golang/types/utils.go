package types

import "math"

const straight = 180

// Converts Degrees to Radians.
func Degs2Rads[T Float](x T) T {
	return x * (math.Pi / straight)
}

// Converts Radians to Degrees.
func Rads2Degs[T Float](x T) T {
	return x * (straight / math.Pi)
}

// Tests for approximate equality between two floating-point numbers within an absolute
// or relative tolerance of Epsilon. An absolute tolerance is used for values
// less than or equal to 1.0. A relative tolerance is used for larger values.
func ApproxEq[T Float](x, y T) bool {
	a := float64(x)
	b := float64(y)
	return math.Abs(a-b) <= Tolerance*math.Max(1.0, math.Max(math.Abs(a), math.Abs(b)))
}
