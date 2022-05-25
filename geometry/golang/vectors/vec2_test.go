package vectors

import (
	"testing"
)

func TestStringer(t *testing.T) {
	v := Vector2d{3.0, 4.0}
	expect := "vec(3.00, 4.00)"
	result := v.String()
	if expect != result {
		t.Errorf("Test stringer failed. Expected: %q, Got: %q", expect, result)
	}
}

func TestFlip(t *testing.T) {
	v := Vector2d{3.0, 4.0}
	expect := Vector2d{-3.0, -4.0}
	result := v.Flip()
	if expect != result {
		t.Errorf("Test Flip failed. Expected: %v, Got %v", expect, result)
	}
}

func TestMagnitude(t *testing.T) {
	v := Vector2d{3.0, 4.0}
	expect := 5.0
	result := v.Magnitude()
	if expect != result {
		t.Errorf("Test Magnitude failed. Expected: %.2f, Got: %.2f", expect, result)
	}
}

func TestScale(t *testing.T) {
	v := Vector2d{3.0, 4.0}
	s := 11.0
	expect := Vector2d{33.0, 44.0}
	result := v.Scale(s)
	if expect != result {
		t.Errorf("Test Scale failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestAdd(t *testing.T) {
	v := Vector2d{1.0, 2.0}
	expect := Vector2d{11.0, 17.0}
	result := v.Add(Vector2d{3.0, 4.0}, Vector2d{7.0, 11.0})
	if expect != result {
		t.Errorf("Test Add failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestDotProduct(t *testing.T) {
	v1 := Vector2d{3.0, 4.0}
	v2 := Vector2d{1.0, 2.0}
	expect := 11.0
	result := v1.Dot(v2)
	if expect != result {
		t.Errorf("Test Dot Product failed. Expected: %.2f, Got: %.2f", expect, result)
	}
}

func TestRound(t *testing.T) {
	v := Vector2d{0.75, 4.25}
	expect := Vector2d{1.0, 4.0}
	result := v.Round()
	if expect != result {
		t.Errorf("Test Cross Product failed. Expected: %.2f, Got: %.2f", expect, result)
	}
}
