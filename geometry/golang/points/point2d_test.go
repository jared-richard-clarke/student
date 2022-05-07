package points

import (
	"testing"
)

func TestStringer(t *testing.T) {
	p := Point{3.0, 4.0}
	expect := "pt(3.00, 4.00)"
	result := p.String()
	if expect != result {
		t.Errorf("Test Stringer failed. Expected: %s, Got: %s", expect, result)
	}
}

func TestSegment(t *testing.T) {
	p1 := Point{2.0, 0.0}
	p2 := Point{3.0, 0.0}
	expect := 1.0
	result := p1.Segment(p2)
	if expect != result {
		t.Errorf("Test Segment failed. Expected %.2f, Got: %.2f", expect, result)
	}
}

func TestPathLength(t *testing.T) {
	pth := Path{{1.0, 1.0}, {5.0, 1.0}, {5.0, 4.0}, {1.0, 1.0}}
	expect := 12.0
	result := pth.Length()
	if expect != result {
		t.Errorf("Test Path Length failed. Expected: %.2f, Got: %.2f", expect, result)
	}
}

func TestRound(t *testing.T) {
	p := Point{0.55, 1.75}
	expect := Point{1.0, 2.0}
	result := p.Round()
	if expect != result {
		t.Errorf("Test Round failed. Expected: %v, Got: %v", expect, result)
	}
}
