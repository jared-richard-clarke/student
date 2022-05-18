package angles

import "testing"

func TestDegreeToRadians(t *testing.T) {
	expect := Radians(0.017453292519943295)
	result := DegreesToRadians(1.0)
	if expect != result {
		t.Errorf("Test DegreesToRadians failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestRadiansToDegrees(t *testing.T) {
	expect := Degrees(1.0)
	result := RadiansToDegrees(0.017453292519943295)
	if expect != result {
		t.Errorf("Test RadiansToDegrees failed. Expected: %v, Got: %v", expect, result)
	}
}
