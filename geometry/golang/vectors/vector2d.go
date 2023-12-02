package vectors

import (
	"didact/geometry/golang/matrices"
	"didact/geometry/golang/types"
	"fmt"
	"math"
)

// A two-dimensional cartesian vector.
type Vec2[T types.Float] struct{ X, Y T }

// As opposed to operator "==", function "AboutEqual" tests whether
// floating-point, 2D vector components are approximately equal.
func (v1 Vec2[T]) AboutEqual(v2 Vec2[T]) bool {
	eq := types.ApproxEq[T]
	x1 := v1.X
	y1 := v1.Y
	x2 := v2.X
	y2 := v2.Y
	return eq(x1, x2) && eq(y1, y2)
}

// Returns a 2D vector with the absolute values of its components.
func (v Vec2[T]) Absolute() Vec2[T] {
	x := T(math.Abs(float64(v.X)))
	y := T(math.Abs(float64(v.Y)))
	return Vec2[T]{x, y}
}

// Returns a 2D vector that is the sum of two 2D vectors.
func (v1 Vec2[T]) Add(v2 Vec2[T]) Vec2[T] {
	x1 := v1.X
	y1 := v1.Y
	x2 := v2.X
	y2 := v2.Y
	return Vec2[T]{x1 + x2, y1 + y2}
}

// Returns a 2D vector that is the difference of two 2D vectors.
func (v1 Vec2[T]) Subtract(v2 Vec2[T]) Vec2[T] {
	x1 := v1.X
	y1 := v1.Y
	x2 := v2.X
	y2 := v2.Y
	return Vec2[T]{x1 - x2, y1 - y2}
}

// Flips the signs of the 2D vector components.
func (v Vec2[T]) Negate() Vec2[T] {
	x := v.X
	y := v.Y
	return Vec2[T]{-x, -y}
}

// Inverts the 2D vector components.
func (v Vec2[T]) Invert() Vec2[T] {
	x := v.X
	y := v.Y
	one := T(1)
	return Vec2[T]{one / x, one / y}
}

// Returns the magnitude of a 2D vector.
func (v Vec2[T]) Magnitude() T {
	x := float64(v.X)
	y := float64(v.Y)
	return T(math.Hypot(x, y))
}

// Multiplies a 2D vector by a scalar.
func (v Vec2[T]) Scale(s T) Vec2[T] {
	x := v.X
	y := v.Y
	return Vec2[T]{x * s, y * s}
}

// Computes the dot product of two 2D vectors.
func (v1 Vec2[T]) DotProduct(v2 Vec2[T]) T {
	x1 := v1.X
	y1 := v1.Y
	x2 := v2.X
	y2 := v2.Y
	return x1*x2 + y1*y2
}

// Calculates the distance between two 2D vector points.
func (v1 Vec2[T]) Distance(v2 Vec2[T]) T {
	return v2.Subtract(v1).Magnitude()
}

// Interpolates the components of the two 2D vectors.
func (v1 Vec2[T]) Interpolate(t T, v2 Vec2[T]) Vec2[T] {
	return v1.Add(v2.Subtract(v1).Scale(t))
}

// Returns the unit vector of a 2D vector.
func (v Vec2[T]) Normalize() Vec2[T] {
	mag := v.Magnitude()
	x := v.X
	y := v.Y
	return Vec2[T]{x / mag, y / mag}
}

// Returns a 2D vector with its components rounded.
func (v Vec2[T]) Round() Vec2[T] {
	x := T(math.Round(float64(v.X)))
	y := T(math.Round(float64(v.Y)))
	return Vec2[T]{x, y}
}

func (v Vec2[T]) TransformBy(m matrices.Mat3[T]) Vec2[T] {
	return Vec2[T]{
		m.A*v.X + m.C*v.Y,
		m.B*v.X + m.D*v.Y,
	}
}

// Fulfills the Stringer interface for Vec2.
func (v Vec2[T]) String() string {
	x := v.X
	y := v.Y
	return fmt.Sprintf("Vec2(%v, %v)", x, y)
}
