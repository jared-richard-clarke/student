package points

import (
	"didact/geometry/golang/matrices"
	"didact/geometry/golang/vectors"
	"testing"
)

func TestAdd(t *testing.T) {
	expect := Pt2{4.0, 6.0}
	result := Pt2{3.0, 4.0}.Add(vectors.Vec2{1.0, 2.0})
	if expect != result {
		t.Errorf("Test Add Pt2 failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestSub(t *testing.T) {
	expect := vectors.Vec2{-1.0, -2.0}
	result := Pt2{3.0, 4.0}.Sub(Pt2{4.0, 6.0})
	if expect != result {
		t.Errorf("Test Sub Pt2 failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestDistance(t *testing.T) {
	expect := 5.0
	result := Pt2{4.0, 6.0}.Distance(Pt2{1.0, 2.0})
	if expect != result {
		t.Errorf("Test Distance Pt2 failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestLerp(t *testing.T) {
	expect := Pt2{2.0, 1.5}
	result := Pt2{1.0, 1.0}.Lerp(0.5, Pt2{3.0, 2.0})
	if expect != result {
		t.Errorf("Test Lerp Pt2 failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestTransformBy(t *testing.T) {
	mat := matrices.Translate(3.0, 4.0)
	expect := Pt2{3.0, 4.0}
	result := Pt2{0.0, 0.0}.TransformBy(mat)
	if expect != result {
		t.Errorf("Test Pt2 TransformBy failed. Expected: %v, Got: %v", expect, result)
	}
}
