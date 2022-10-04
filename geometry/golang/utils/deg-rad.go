package utils

import (
	"math"
)

// Converts Degrees to Radians.
func Degs2Rads(d float64) float64 {
	return d * (math.Pi / 180.0)
}

// Converts Radians to Degrees.
func Rads2Degs(r float64) float64 {
	return r * (180 / math.Pi)
}
