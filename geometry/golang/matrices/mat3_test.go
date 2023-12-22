package matrices

import (
	utils "didact/geometry/golang/utilities"
	"testing"
)

func TestApproxEq(t *testing.T) {
	ep := utils.Tolerance
	m1 := Mat3{1 + ep, 1 + ep, 1 + ep, 1 + ep, 1 + ep, 1 + ep}
	m2 := Mat3{1, 1, 1, 1, 1, 1}
	if m1.ApproxEq(m2) != true {
		t.Errorf("Test ApproxEq failed. Check test epsilon (ep).")
	}
	m1 = Mat3{1 + ep*10, 1 + ep, 1 + ep, 1 + ep, 1 + ep, 1 + ep}
	if m1.ApproxEq(m2) == true {
		t.Errorf("Test not ApproxEq failed. Check test epsilon (ep).")
	}
}

func TestIdentity(t *testing.T) {
	expect := Mat3{1, 0, 0, 1, 0, 0}
	result := Identity()
	if expect != result {
		t.Errorf("Test Identity failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestTranslate(t *testing.T) {
	expect := Mat3{1, 0, 0, 1, 3, 4}
	result := Identity().Translate(3, 4)
	if expect != result {
		t.Errorf("Test Translate failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestScale(t *testing.T) {
	expect := Mat3{3, 0, 0, 4, 0, 0}
	result := Identity().Scale(3, 4)
	if expect != result {
		t.Errorf("Test Scale failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestRotate(t *testing.T) {
	expect := Identity()
	result := Identity().Rotate(utils.DegreesToRadians(90.0)).Rotate(utils.DegreesToRadians(-90.0))
	if expect != result {
		t.Errorf("Test Rotate failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestShear(t *testing.T) {
	expect := Mat3{1, 4, 3, 1, 0, 0}
	result := Identity().Shear(3, 4)
	if expect != result {
		t.Errorf("Test Shear failed. Expected: %v, Got: %v", expect, result)
	}
}
