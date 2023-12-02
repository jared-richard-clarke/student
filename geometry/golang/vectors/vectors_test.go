package vectors

import (
	"didact/geometry/golang/matrices"
	"didact/geometry/golang/types"
	"testing"
)

type vector = Vec2[float64]

func TestApproxEq(t *testing.T) {
	tolerance := types.Tolerance

	v1 := vector{3 + tolerance, 4 + tolerance}
	v2 := vector{3, 4}
	if v1.AboutEqual(v2) == false {
		t.Errorf("Test ApproxEq vector failed: false negative. Check tolerance in test.")
	}
	// Push values past threshold for approximate equality
	// by pushing epsilon into one figure higher in significance.
	v1 = vector{3 + tolerance*10, 4 + tolerance}
	if v1.AboutEqual(v2) == true {
		t.Errorf("Test ApproxEq vector failed: false positive. Check tolerance in test.")
	}
}

func TestAbsolute(t *testing.T) {
	expect := vector{3, 4}
	result := vector{-3, -4}.Absolute()
	if expect != result {
		t.Errorf("Test Absolute vector failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestAdd(t *testing.T) {
	expect := vector{2, 4}
	result := vector{4, 6}.Add(vector{-2, -2})
	if expect != result {
		t.Errorf("Test Add vector failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestSubtract(t *testing.T) {
	expect := vector{2, 2}
	result := vector{3, 4}.Subtract(vector{1, 2})
	if expect != result {
		t.Errorf("Test Subtract vector failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestNegate(t *testing.T) {
	expect := vector{-3, -4}
	result := vector{3, 4}.Negate()
	if expect != result {
		t.Errorf("Test Negate vector failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestInvert(t *testing.T) {
	expect := vector{0.5, 0.5}
	result := vector{2, 2}.Invert()
	if result != expect {
		t.Errorf("Test Invert vector failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestMagnitude(t *testing.T) {
	expect := 5.0
	result := vector{3.0, 4.0}.Magnitude()
	if expect != result {
		t.Errorf("Test Magnitude vector failed. Expected: %.2f, Got: %.2f", expect, result)
	}
}

func TestScale(t *testing.T) {
	expect := vector{1, 0.5}
	result := vector{0.5, 0.25}.Scale(2.0)
	if expect != result {
		t.Errorf("Test Scale vector failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestDotProduct(t *testing.T) {
	expect := 11.0
	result := vector{3, 4}.DotProduct(vector{1, 2})
	if expect != result {
		t.Errorf("Test Dot Product vector failed. Expected: %.2f, Got: %.2f", expect, result)
	}
}

func TestDistance(t *testing.T) {
	expect := 7.0
	result := vector{8, 0}.Distance(vector{1, 0})
	if expect != result {
		t.Errorf("Test Distance vector failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestLerp(t *testing.T) {
	expect := vector{2, 1.5}
	result := vector{1, 1}.Lerp(0.5, vector{3, 2})
	if expect != result {
		t.Errorf("Test Lerp vector failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestNormalize(t *testing.T) {
	expect := vector{0.6, 0.8}
	result := vector{3, 4}.Normalize()
	if expect != result {
		t.Errorf("Test Normalize vector failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestRound(t *testing.T) {
	expect := vector{1, 4}
	result := vector{0.75, 4.25}.Round()
	if expect != result {
		t.Errorf("Test Round vector failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestTransformBy(t *testing.T) {
	mat := matrices.Scale[float64](10, 10)
	expect := vector{30, 40}
	result := vector{3, 4}.TransformBy(mat)
	if expect != result {
		t.Errorf("Test vector TransformBy failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestStringer(t *testing.T) {
	expect := "Vec2(3, 4)"
	result := vector{3.0, 4.0}.String()
	if expect != result {
		t.Errorf("Test Stringer vector failed. Expected: %v, Got: %v", expect, result)
	}
}
