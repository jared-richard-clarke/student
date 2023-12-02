// Provides number types and constants.
package types

// An arbitrary maximum allowable difference in precision between floating-point numbers.
const Tolerance = 1e-07

// The numerical components of vectors and matrices are limited to either
// 64-bit or 32-bit floating point.
type Float interface {
	float32 | float64
}
