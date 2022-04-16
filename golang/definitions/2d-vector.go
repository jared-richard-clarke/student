package definitions

import (
	"fmt"
	"math"
)

// A two-dimensional quantity that has direction and magnitude. Represented by coordinates X and Y.
type Vector struct {
	X, Y float64
}

// Fulfills the Stringer interface for the fmt package.
func (v Vector) String() string {
	x := v.X
	y := v.Y
	return fmt.Sprintf("(%.2f, %.2f)", x, y)
}

// Computes the distance of a 2d-vector's point from the origin.
func (v Vector) Magnitude() float64 {
	x := v.X
	y := v.Y
	return math.Hypot(x, y)
}

// Computes the unit vector of a 2d-vector. Unit vectors have a magnitude of 1.
func (v Vector) UnitVector() Vector {
	x := v.X
	y := v.Y
	m := math.Hypot(x, y)
	return Vector{x / m, y / m}
}

// Scales a 2d-vector by a factor of n.
func Scale(v Vector, n float64) Vector {
	x := v.X
	y := v.Y
	return Vector{x * n, y * n}
}

// Computes the sum of two 2d-vectors.
func Add(v1, v2 Vector) Vector {
	x1 := v1.X
	y1 := v1.Y
	x2 := v2.X
	y2 := v2.Y
	return Vector{x1 + x2, y1 + y2}
}

// Computes the dot product of two 2d-vectors.
func DotProduct(v1, v2 Vector) float64 {
	x1 := v1.X
	y1 := v1.Y
	x2 := v2.X
	y2 := v2.Y
	return x1*x2 + y1*y2
}

// Generates functions for comparing the magnitudes of two 2d-vectors.
func compare(op func(float64, float64) bool) func(Vector, Vector) bool {
	return func(v1, v2 Vector) bool {
		m1 := v1.Magnitude()
		m2 := v2.Magnitude()
		return op(m1, m2)
	}
}

// 2d-Vector comparison functions
var Gt = compare(func(x, y float64) bool { return x > y })
var Lt = compare(func(x, y float64) bool { return x < y })
var Eq = compare(func(x, y float64) bool { return x == y })

// var hypotenuse = func() func(v Vector) float64 {
//     cache := make(map[Vector]float64)
//     return func(v Vector) float64 {
//         if n, ok := cache[v]; ok {
//             return n
//         }
//         h := math.Hypot(v.X, v.Y)
//         cache[v] = h
//         return h
//     }
// }()

