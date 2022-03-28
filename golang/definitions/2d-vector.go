package definitions

import "math"

// A two-dimensional quantity that has direction and magnitude. Represented by coordinates X and Y.
type Vector struct {
	X, Y float64
}

// Fulfills the Stringer interface for the fmt package.
func (v Vector) String() string {
	return fmt.Sprintf("(%.2f, %.2f)", v.X, v.Y)
}

// Computes the distance of a 2d-vector's point from the origin.
func (v Vector) Magnitude() float64 {
	return hypot(v)
}

// Computes the unit vector of a 2d-vector. Unit vectors have a magnitude of 1.
func (v Vector) UnitVector() Vector {
	x := v.X
	y := v.Y
	m := hypot(v)
	return Vector{x / m, y / m}
}

// Scales a 2d-vector by a factor of n.
func Scale(v Vector, n float64) Vector {
	return Vector{v.X * n, v.Y * n}
}

// Computes the sum of two 2d-vectors.
func Add(v1, v2 Vector) Vector {
	return Vector{v1.X + v2.X, v1.Y + v2.Y}
}

// Computes the dot product of two 2d-vectors.
func DotProduct(v1, v2 Vector) float64 {
	x1 := v1.X
	y1 := v1.Y
	x2 := v2.X
	y2 := v2.Y
	return x1*x2 + y1*y2
}

// Finding the hypotenuse is computationally expensive.
// Function hypot caches result to reduce redundant computation.
var hypot = func(h func(float64, float64) float64) func(v Vector) float64 {
	cache := make(map[Vector]float64)
	return func(v Vector) float64 {
		if n, ok := cache[v]; ok {
			return n
		}
		hypotenuse := h(v.X, v.Y)
		cache[v] = hypotenuse
		return hypotenuse
	}
}(math.Hypot)
