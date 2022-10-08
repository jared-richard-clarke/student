package matrices

import (
	"didact/geometry/golang/utils"
	"didact/geometry/golang/vectors"
	"testing"
)

func TestApproxEq(t *testing.T) {
	ep := 0.000001
	m1 := Mat3{1.0 + ep, 1.0 + ep, 1.0 + ep, 1.0 + ep, 1.0 + ep, 1.0 + ep}
	m2 := Mat3{1.0, 1.0, 1.0, 1.0, 1.0, 1.0}
	if m1.ApproxEq(m2) != true {
		t.Errorf("Test ApproxEq failed. Check test epsilon (ep).")
	}
	m1 = Mat3{1.0 + ep*10, 1.0 + ep, 1.0 + ep, 1.0 + ep, 1.0 + ep, 1.0 + ep}
	if m1.ApproxEq(m2) == true {
		t.Errorf("Test not ApproxEq failed. Check test epsilon (ep).")
	}
}

func TestIdentity(t *testing.T) {
	expect := Mat3{1.0, 0.0, 0.0, 1.0, 0.0, 0.0}
	result := Identity()
	if expect != result {
		t.Errorf("Test Identity failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestTranslate(t *testing.T) {
	expect := Mat3{1.0, 0.0, 0.0, 1.0, 3.0, 4.0}
	result := Identity().Translate(3.0, 4.0)
	if expect != result {
		t.Errorf("Test Translate failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestScale(t *testing.T) {
	expect := Mat3{3.0, 0.0, 0.0, 4.0, 0.0, 0.0}
	result := Identity().Scale(3.0, 4.0)
	if expect != result {
		t.Errorf("Test Scale failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestRotate(t *testing.T) {
	expect := Identity()
	result := Identity().Rotate(utils.Degs2Rads(90.0)).Rotate(utils.Degs2Rads(-90.0))
	if expect != result {
		t.Errorf("Test Rotate failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestShear(t *testing.T) {
	expect := Mat3{1.0, 4.0, 3.0, 1.0, 0.0, 0.0}
	result := Identity().Shear(3.0, 4.0)
	if expect != result {
		t.Errorf("Test Shear failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestTransformVector(t *testing.T) {
	expect := vectors.Vec2{6.0, 8.0}
	result := Identity().Scale(2.0, 2.0).TransformVector(vectors.Vec2{3.0, 4.0})
	if expect != result {
		t.Errorf("Test TransformVector failed. Expected: %v, Got: %v", expect, result)
	}
}
