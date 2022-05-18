package angles

import "testing"

func TestDegreesToString(t *testing.T) {
	expect := "30.000000°"
	result := Degrees(30.0).String()
	if expect != result {
		t.Errorf("Test DegreesToString failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestRadiansToString(t *testing.T) {
	expect := "30.000000rad"
	result := Radians(30.0).String()
	if expect != result {
		t.Errorf("Test RadiansToString failed. Expected: %v, Got: %v", expect, result)
	}
}

func TestDegreesToRadians(t *testing.T) {
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
