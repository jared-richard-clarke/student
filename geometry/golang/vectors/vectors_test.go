package vectors

import (
	"testing"
)

func TestApproxEq(t *testing.T) {
	ep := 0.000001

	v1 := Vec2{3.0 + ep, 4.0 + ep}
	v2 := Vec2{3.0, 4.0}
	if v1.ApproxEq(v2) == false {
		t.Errorf("Test ApproxEq Vec2 failed: false negative. Check epsilon (ep) in test.")
	}
	// Push values past threshold for approximate equality
	// by pushing epsilon into one figure higher in significance.
	v1 = Vec2{3.0 + ep*10, 4.0 + ep}
	if v1.ApproxEq(v2) == true {
		t.Errorf("Test ApproxEq Vec2 failed: false positive. Check epsilon (ep) in test.")
	}
}

func TestAbs(t *testing.T) {
	expect := Vec2{3.0, 4.0}
	result := Vec2{-3.0, -4.0}.Abs()
	if expect != result {
		t.Errorf("Test Abs Vec2 failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestAdd(t *testing.T) {
	expect := Vec2{2.0, 4.0}
	result := Vec2{4.0, 6.0}.Add(Vec2{-2.0, -2.0})
	if expect != result {
		t.Errorf("Test Add Vec2 failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestSub(t *testing.T) {
	expect := Vec2{2.0, 2.0}
	result := Vec2{3.0, 4.0}.Sub(Vec2{1.0, 2.0})
	if expect != result {
		t.Errorf("Test Sub Vec2 failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestNegate(t *testing.T) {
	expect := Vec2{-3.0, -4.0}
	result := Vec2{3.0, 4.0}.Negate()
	if expect != result {
		t.Errorf("Test Negate Vec2 failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestInvert(t *testing.T) {
	expect := Vec2{0.5, 0.5}
	result := Vec2{2.0, 2.0}.Invert()
	if result != expect {
		t.Errorf("Test Invert Vec2 failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestMag(t *testing.T) {
	vec2Expect := 5.0
	vec2Result := Vec2{3.0, 4.0}.Mag()
	if vec2Expect != vec2Result {
		t.Errorf("Test Mag Vec2 failed. Expected: %.2f, Got: %.2f", vec2Expect, vec2Result)
	}
}

func TestScale(t *testing.T) {
	expect := Vec2{1.0, 0.5}
	result := Vec2{0.5, 0.25}.Scale(2.0)
	if expect != result {
		t.Errorf("Test Scale Vec2 failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestDotProduct(t *testing.T) {
	expect := 11.0
	result := Vec2{3.0, 4.0}.Dot(Vec2{1.0, 2.0})
	if expect != result {
		t.Errorf("Test Dot Product Vec2 failed. Expected: %.2f, Got: %.2f", expect, result)
	}
}

func TestDistance(t *testing.T) {
	expect := 7.0
	result := Vec2{8.0, 0.0}.Distance(Vec2{1.0, 0.0})
	if expect != result {
		t.Errorf("Test Distance Vec2 failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestLerp(t *testing.T) {
	expect := Vec2{2.0, 1.5}
	result := Vec2{1.0, 1.0}.Lerp(0.5, Vec2{3.0, 2.0})
	if expect != result {
		t.Errorf("Test Lerp Vec2 failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestNormalize(t *testing.T) {
	expect := Vec2{0.6, 0.8}
	result := Vec2{3.0, 4.0}.Normalize()
	if expect != result {
		t.Errorf("Test Normalize Vec2 failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestRound(t *testing.T) {
	expect := Vec2{1.0, 4.0}
	result := Vec2{0.75, 4.25}.Round()
	if expect != result {
		t.Errorf("Test Round Vec2 failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestStringer(t *testing.T) {
	expect := "vec2(3.00, 4.00)"
	result := Vec2{3.0, 4.0}.String()
	if expect != result {
		t.Errorf("Test Stringer Vec2 failed. Expected: %v, Got: %v", expect, result)
	}
}

