package vectors

import (
	"didact/geometry/golang/matrices"
	utils "didact/geometry/golang/utilities"
	"testing"
)

func TestApproxEq(t *testing.T) {
	tolerance := utils.Tolerance

	v1 := Vec2{3 + tolerance, 4 + tolerance}
	v2 := Vec2{3, 4}
	if v1.AboutEqual(v2) == false {
		t.Errorf("Test ApproxEq Vec2 failed: false negative. Check tolerance in test.")
	}
	// Push values past threshold for approximate equality
	// by pushing epsilon into one figure higher in significance.
	v1 = Vec2{3 + tolerance*10, 4 + tolerance}
	if v1.AboutEqual(v2) == true {
		t.Errorf("Test ApproxEq Vec2 failed: false positive. Check tolerance in test.")
	}
}

func TestAbsolute(t *testing.T) {
	expect := Vec2{3, 4}
	result := Vec2{-3, -4}.Absolute()
	if expect != result {
		t.Errorf("Test Absolute Vec2 failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestAdd(t *testing.T) {
	expect := Vec2{2, 4}
	result := Vec2{4, 6}.Add(Vec2{-2, -2})
	if expect != result {
		t.Errorf("Test Add Vec2 failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestSubtract(t *testing.T) {
	expect := Vec2{2, 2}
	result := Vec2{3, 4}.Subtract(Vec2{1, 2})
	if expect != result {
		t.Errorf("Test Subtract Vec2 failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestNegate(t *testing.T) {
	expect := Vec2{-3, -4}
	result := Vec2{3, 4}.Negate()
	if expect != result {
		t.Errorf("Test Negate Vec2 failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestInvert(t *testing.T) {
	expect := Vec2{0.5, 0.5}
	result := Vec2{2, 2}.Invert()
	if result != expect {
		t.Errorf("Test Invert Vec2 failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestMagnitude(t *testing.T) {
	expect := 5.0
	result := Vec2{3.0, 4.0}.Magnitude()
	if expect != result {
		t.Errorf("Test Magnitude Vec2 failed. Expected: %.2f, Got: %.2f", expect, result)
	}
}

func TestScale(t *testing.T) {
	expect := Vec2{1, 0.5}
	result := Vec2{0.5, 0.25}.Scale(2.0)
	if expect != result {
		t.Errorf("Test Scale Vec2 failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestDotProduct(t *testing.T) {
	expect := 11.0
	result := Vec2{3, 4}.DotProduct(Vec2{1, 2})
	if expect != result {
		t.Errorf("Test Dot Product Vec2 failed. Expected: %.2f, Got: %.2f", expect, result)
	}
}

func TestDistance(t *testing.T) {
	expect := 7.0
	result := Vec2{8, 0}.Distance(Vec2{1, 0})
	if expect != result {
		t.Errorf("Test Distance Vec2 failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestInterpolate(t *testing.T) {
	expect := Vec2{2, 1.5}
	result := Vec2{1, 1}.Interpolate(0.5, Vec2{3, 2})
	if expect != result {
		t.Errorf("Test Interpolate Vec2 failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestNormalize(t *testing.T) {
	expect := Vec2{0.6, 0.8}
	result := Vec2{3, 4}.Normalize()
	if expect != result {
		t.Errorf("Test Normalize Vec2 failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestRound(t *testing.T) {
	expect := Vec2{1, 4}
	result := Vec2{0.75, 4.25}.Round()
	if expect != result {
		t.Errorf("Test Round Vec2 failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestTransformBy(t *testing.T) {
	mat := matrices.Scale(10, 10)
	expect := Vec2{30, 40}
	result := Vec2{3, 4}.TransformBy(mat)
	if expect != result {
		t.Errorf("Test Vec2 TransformBy failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestStringer(t *testing.T) {
	expect := "Vec2(3, 4)"
	result := Vec2{3.0, 4.0}.String()
	if expect != result {
		t.Errorf("Test Stringer Vec2 failed. Expected: %v, Got: %v", expect, result)
	}
}
