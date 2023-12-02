package matrices

import (
	"didact/geometry/golang/types"
	"testing"
)

type matrix = Mat3[float64]

func TestApproxEq(t *testing.T) {
	ep := types.Tolerance
	m1 := matrix{1 + ep, 1 + ep, 1 + ep, 1 + ep, 1 + ep, 1 + ep}
	m2 := matrix{1, 1, 1, 1, 1, 1}
	if m1.ApproxEq(m2) != true {
		t.Errorf("Test ApproxEq failed. Check test epsilon (ep).")
	}
	m1 = matrix{1 + ep*10, 1 + ep, 1 + ep, 1 + ep, 1 + ep, 1 + ep}
	if m1.ApproxEq(m2) == true {
		t.Errorf("Test not ApproxEq failed. Check test epsilon (ep).")
	}
}

func TestIdentity(t *testing.T) {
	expect := matrix{1, 0, 0, 1, 0, 0}
	result := Identity[float64]()
	if expect != result {
		t.Errorf("Test Identity failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestTranslate(t *testing.T) {
	expect := matrix{1, 0, 0, 1, 3, 4}
	result := Identity[float64]().Translate(3, 4)
	if expect != result {
		t.Errorf("Test Translate failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestScale(t *testing.T) {
	expect := matrix{3, 0, 0, 4, 0, 0}
	result := Identity[float64]().Scale(3, 4)
	if expect != result {
		t.Errorf("Test Scale failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestRotate(t *testing.T) {
	expect := Identity[float64]()
	result := Identity[float64]().Rotate(types.Degs2Rads(90.0)).Rotate(types.Degs2Rads(-90.0))
	if expect != result {
		t.Errorf("Test Rotate failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestShear(t *testing.T) {
	expect := matrix{1, 4, 3, 1, 0, 0}
	result := Identity[float64]().Shear(3, 4)
	if expect != result {
		t.Errorf("Test Shear failed. Expected: %v, Got: %v", expect, result)
	}
}
