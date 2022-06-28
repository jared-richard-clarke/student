package vectors

import (
	"testing"
)

func TestStringer(t *testing.T) {
	v := Vec2{3.0, 4.0}
	expect := "vec(3.00, 4.00)"
	result := v.String()
	if expect != result {
		t.Errorf("Test stringer failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestAdd(t *testing.T) {
	expect := Vec2{4.0, 6.0}
	result := Vec2{1.0, 2.0}.Add(Vec2{3.0, 4.0})
	if expect != result {
		t.Errorf("Test Add failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestSub(t *testing.T) {
	expect := Vec2{2.0, 2.0}
	result := Vec2{3.0, 4.0}.Sub(Vec2{1.0, 2.0})
	if expect != result {
		t.Errorf("Test Add failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestSum(t *testing.T) {
	expect := Vec2{11.0, 17.0}
	result := Vec2{1.0, 2.0}.Sum(Vec2{3.0, 4.0}, Vec2{7.0, 11.0})
	if expect != result {
		t.Errorf("Test Sum failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestDiff(t *testing.T) {
	v := Vec2{11.0, 17.0}
	expect := Vec2{1.0, 2.0}
	result := v.Diff(Vec2{7.0, 11.0}, Vec2{3.0, 4.0})
	if expect != result {
		t.Errorf("Test Diff failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestNegate(t *testing.T) {
	v := Vec2{3.0, 4.0}
	expect := Vec2{-3.0, -4.0}
	result := v.Negate()
	if expect != result {
		t.Errorf("Test Negate failed. Expected: %v, Got %v", expect, result)
	}
}

func TestScale(t *testing.T) {
	v := Vec2{3.0, 4.0}
	s := 11.0
	expect := Vec2{33.0, 44.0}
	result := v.Scale(s)
	if expect != result {
		t.Errorf("Test Scale failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestDotProduct(t *testing.T) {
	v1 := Vec2{3.0, 4.0}
	v2 := Vec2{1.0, 2.0}
	expect := 11.0
	result := v1.Dot(v2)
	if expect != result {
		t.Errorf("Test Dot Product failed. Expected: %.2f, Got: %.2f", expect, result)
	}
}

func TestNormalize(t *testing.T) {
	v := Vec2{3.0, 4.0}
	expect := Vec2{0.6, 0.8}
	result := v.Normalize()
	if expect != result {
		t.Errorf("Test Normalize failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestMagnitude(t *testing.T) {
	v := Vec2{3.0, 4.0}
	expect := 5.0
	result := v.Magnitude()
	if expect != result {
		t.Errorf("Test Magnitude failed. Expected: %.2f, Got: %.2f", expect, result)
	}
}

func TestDistance(t *testing.T) {
	v1 := Vec2{8.0, 0.0}
	v2 := Vec2{1.0, 0.0}
	expect := 7.0
	result := v1.Distance(v2)
	if expect != result {
		t.Errorf("Test Distance failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestLerp(t *testing.T) {
	v1 := Vec2{0.0, 10.0}
	v2 := Vec2{8.0, -4.0}
	expect := Vec2{-8.0, 24.0}
	result := v1.Lerp(v2, -1.0)
	if expect != result {
		t.Errorf("Test Lerp failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestApproxEq(t *testing.T) {
	v1 := Vec2{3.2, 4.0}
	v2 := Vec2{3.19999999989, 4.0}
	expect := true
	result := v1.ApproxEq(v2)
	if expect != result {
		t.Errorf("Test ApproxEq failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestRound(t *testing.T) {
	v := Vec2{0.75, 4.25}
	expect := Vec2{1.0, 4.0}
	result := v.Round()
	if expect != result {
		t.Errorf("Test Round failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestCeil(t *testing.T) {
	expect := Vec2{1.0, 5.0}
	result := Vec2{0.75, 4.25}.Ceil()
	if expect != result {
		t.Errorf("Test Ceil failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestFloor(t *testing.T) {
	expect := Vec2{0.0, 4.0}
	result := Vec2{0.75, 4.25}.Floor()
	if expect != result {
		t.Errorf("Test Floor failed. Expected: %v, Got: %v", expect, result)
	}
}
