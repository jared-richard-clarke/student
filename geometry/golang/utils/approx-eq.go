package utils

import "math"

// An arbitrary maximum allowable difference in precision between floating-point numbers.
const Tolerance = 1e-07

// Tests for approximate equality between two 64-bit, floating-point numbers within an absolute
// or relative tolerance of Epsilon. An absolute tolerance is used for values
// less than or equal to 1.0. A relative tolerance is used for larger values.
func ApproxEq(x, y float64) bool {
	return math.Abs(x-y) <= Tolerance*math.Max(1.0, math.Max(math.Abs(x), math.Abs(y)))
}
