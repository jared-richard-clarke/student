package utils

import "testing"

func TestApproxEq(t *testing.T) {
	expect := true
	result := ApproxEq(0.2, 0.19999999989)
	if expect != result {
		t.Errorf("Test ApproxEq failed. Expected: %v, Got: %v", expect, result)
	}
}
