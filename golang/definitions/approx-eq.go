package definitions

import "math"

// The maximum allowable difference in precision between floating-point numbers.
const Epsilon = 0.000001

// Tests for approximate equality between two floating-point numbers within an absolute
// or relative tolerance of Epsilon. An absolute tolerance is used for values
// less than or equal to 1.0. A relative tolerance is used for larger values.
func ApproxEq(x, y float64) bool {
	return math.Abs(x-y) <= Epsilon*math.Max(1.0, math.Max(math.Abs(x), math.Abs(y)))
}
