package utils

import "testing"

func TestDegs2Rads(t *testing.T) {
	expect := 0.017453292519943295
	result := Degs2Rads(1.0)
	if expect != result {
		t.Errorf("Test Degs2Rads failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestRads2Degs(t *testing.T) {
	expect := 1.0
	result := Rads2Degs(0.017453292519943295)
	if expect != result {
		t.Errorf("Test Rads2Degs failed. Expected: %v, Got: %v", expect, result)
	}
}
