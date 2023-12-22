package utilities

import "math"

// An arbitrary maximum allowable difference in precision between
// floating-point numbers.
const Tolerance = 1e-07

// Converts Degrees to Radians.
func DegreesToRadians(x float64) float64 {
	return x * (math.Pi / 180)
}

// Converts Radians to Degrees.
func RadiansToDegrees(x float64) float64 {
	return x * (180 / math.Pi)
}

// Tests for approximate equality between two floating-point numbers within an absolute
// or relative tolerance of Epsilon. An absolute tolerance is used for values
// less than or equal to 1.0. A relative tolerance is used for larger values.
func ApproxEq(x float64, y float64) bool {
	return math.Abs(x-y) <= Tolerance*math.Max(1.0, math.Max(math.Abs(x), math.Abs(y)))
}
