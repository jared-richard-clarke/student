package vectors

import (
	"testing"
)

type testVec2 []struct{ expect, result Vec2 }
type testVec3 []struct{ expect, result Vec3 }

func TestApproxEq(t *testing.T) {
	ep := 0.000001

	v1 := Vec2{3.0 + ep, 4.0 + ep}
	v2 := Vec2{3.0, 4.0}
	if v1.ApproxEq(v2) == false {
		t.Errorf("Test ApproxEq Vec2 failed: false negative. Check epsilon (ep) in test.")
	}
	// Push values past threshold for approximate equality
	// by pushing epsilon into one figure higher in significance.
	v1 = Vec2{3.0 + ep*10, 4.0 + ep}
	if v1.ApproxEq(v2) == true {
		t.Errorf("Test ApproxEq Vec2 failed: false positive. Check epsilon (ep) in test.")
	}

	v3 := Vec3{3.0 + ep, 4.0 + ep}
	v4 := Vec3{3.0, 4.0}
	if v3.ApproxEq(v4) == false {
		t.Errorf("Test ApproxEq Vec3 failed: false negative.")
	}
	v3 = Vec3{3.0 + ep*10, 4.0 + ep}
	if v3.ApproxEq(v4) == true {
		t.Errorf("Test ApproxEq Vec3 failed: false positive.")
	}
}

func TestAbs(t *testing.T) {
	absVec2 := testVec2{
		{
			expect: Vec2{3.0, 4.0},
			result: Vec2{-3.0, -4.0}.Abs(),
		},
		{
			expect: Vec2{1.0, 2.0},
			result: Vec2{-1.0, 2.0}.Abs(),
		},
	}
	for _, v := range absVec2 {
		expect := v.expect
		result := v.result
		if expect != result {
			t.Errorf("Test Abs Vec2 failed. Expected: %v, Got: %v", expect, result)
		}
	}
	absVec3 := testVec3{
		{
			expect: Vec3{3.0, 4.0, 5.0},
			result: Vec3{-3.0, -4.0, -5.0}.Abs(),
		},
		{
			expect: Vec3{1.0, 2.0, 3.0},
			result: Vec3{-1.0, 2.0, -3.0}.Abs(),
		},
	}
	for _, v := range absVec3 {
		expect := v.expect
		result := v.result
		if expect != result {
			t.Errorf("Test Abs Vec3 failed. Expected: %v, Got: %v", expect, result)
		}
	}
}

func TestAdd(t *testing.T) {
	addVec2 := testVec2{
		{
			expect: Vec2{4.0, 6.0},
			result: Vec2{1.0, 2.0}.Add(Vec2{3.0, 4.0}),
		},
		{
			expect: Vec2{2.0, 4.0},
			result: Vec2{4.0, 6.0}.Add(Vec2{-2.0, -2.0}),
		},
	}
	for _, v := range addVec2 {
		expect := v.expect
		result := v.result
		if expect != result {
			t.Errorf("Test Add Vec2 failed. Expected: %v, Got: %v", expect, result)
		}
	}
	addVec3 := testVec3{
		{
			expect: Vec3{4.0, 6.0, 8.0},
			result: Vec3{1.0, 1.0, 1.0}.Add(Vec3{3.0, 5.0, 7.0}),
		},
		{
			expect: Vec3{-1.0, 2.0, -5.0},
			result: Vec3{-2.0, 4.0, 0.0}.Add(Vec3{1.0, -2.0, -5.0}),
		},
	}
	for _, v := range addVec3 {
		expect := v.expect
		result := v.result
		if expect != result {
			t.Errorf("Test Add Vec3 failed. Expected: %v, Got: %v", expect, result)
		}
	}
}

func TestSub(t *testing.T) {
	subVec2 := testVec2{
		{
			expect: Vec2{2.0, 2.0},
			result: Vec2{3.0, 4.0}.Sub(Vec2{1.0, 2.0}),
		},
		{
			expect: Vec2{-2.0, -2.0},
			result: Vec2{1.0, 2.0}.Sub(Vec2{3.0, 4.0}),
		},
	}
	for _, v := range subVec2 {
		expect := v.expect
		result := v.result
		if expect != result {
			t.Errorf("Test Sub Vec2 failed. Expected: %v, Got: %v", expect, result)
		}
	}
	subVec3 := testVec3{
		{
			expect: Vec3{0.0, 0.0, 0.0},
			result: Vec3{3.0, 4.0, 5.0}.Sub(Vec3{3.0, 4.0, 5.0}),
		},
		{
			expect: Vec3{3.0, 4.0, 5.0},
			result: Vec3{0.0, 0.0, 0.0}.Sub(Vec3{-3.0, -4.0, -5.0}),
		},
	}
	for _, v := range subVec3 {
		expect := v.expect
		result := v.result
		if expect != result {
			t.Errorf("Test Sub Vec3 failed. Expected: %v, Got: %v", expect, result)
		}
	}
}

func TestNegate(t *testing.T) {
	negateVec2 := testVec2{
		{
			expect: Vec2{-3.0, -4.0},
			result: Vec2{3.0, 4.0}.Negate(),
		},
		{
			expect: Vec2{0.0, 0.0},
			result: Vec2{0.0, 0.0}.Negate(),
		},
	}
	for _, v := range negateVec2 {
		expect := v.expect
		result := v.result
		if expect != result {
			t.Errorf("Test Negate Vec2 failed. Expected: %v, Got: %v", expect, result)
		}
	}
	negateVec3 := testVec3{
		{
			expect: Vec3{-3.0, -4.0, -5.0},
			result: Vec3{3.0, 4.0, 5.0}.Negate(),
		},
		{
			expect: Vec3{1.0, 2.0, 3.0},
			result: Vec3{-1.0, -2.0, -3.0}.Negate(),
		},
	}
	for _, v := range negateVec3 {
		expect := v.expect
		result := v.result
		if expect != result {
			t.Errorf("Test Negate Vec3 failed. Expected: %v, Got: %v", expect, result)
		}
	}
}

func TestInvert(t *testing.T) {
	invertVec2 := testVec2{
		{
			expect: Vec2{0.5, 0.5},
			result: Vec2{2.0, 2.0}.Invert(),
		},
		{
			expect: Vec2{2.0, 2.0},
			result: Vec2{0.5, 0.5}.Invert(),
		},
	}
	for _, v := range invertVec2 {
		expect := v.expect
		result := v.result
		if result != expect {
			t.Errorf("Test Invert Vec2 failed. Expected: %v, Got: %v", expect, result)
		}
	}
	invertVec3 := testVec3{
		{
			expect: Vec3{0.5, 0.5, 0.5},
			result: Vec3{2.0, 2.0, 2.0}.Invert(),
		},
		{
			expect: Vec3{2.0, 2.0, 2.0},
			result: Vec3{0.5, 0.5, 0.5}.Invert(),
		},
	}
	for _, v := range invertVec3 {
		expect := v.expect
		result := v.result
		if result != expect {
			t.Errorf("Test Invert Vec3 failed. Expected: %v, Got: %v", expect, result)
		}
	}
}

func TestMag(t *testing.T) {
	vec2Expect := 5.0
	vec2Result := Vec2{3.0, 4.0}.Mag()
	if vec2Expect != vec2Result {
		t.Errorf("Test Mag Vec2 failed. Expected: %.2f, Got: %.2f", vec2Expect, vec2Result)
	}
	vec3Expect := 3.7416573867739413
	vec3Result := Vec3{1.0, 2.0, 3.0}.Mag()
	if vec3Expect != vec3Result {
		t.Errorf("Test Mag Vec3 failed. Expected: %.2f, Got: %.2f", vec3Expect, vec3Result)
	}
}

func TestScale(t *testing.T) {
	scaleVec2 := testVec2{
		{
			expect: Vec2{33.0, 44.0},
			result: Vec2{3.0, 4.0}.Scale(11.0),
		},
		{
			expect: Vec2{1.0, 0.5},
			result: Vec2{0.5, 0.25}.Scale(2.0),
		},
	}
	for _, v := range scaleVec2 {
		expect := v.expect
		result := v.result
		if expect != result {
			t.Errorf("Test Scale Vec2 failed. Expected: %v, Got: %v", expect, result)
		}
	}
	scaleVec3 := testVec3{
		{
			expect: Vec3{33.0, 44.0, 11.0},
			result: Vec3{3.0, 4.0, 1.0}.Scale(11.0),
		},
		{
			expect: Vec3{2.5, 1.5, 1.0},
			result: Vec3{10.0, 6.0, 4.0}.Scale(0.25),
		},
	}
	for _, v := range scaleVec3 {
		expect := v.expect
		result := v.result
		if expect != result {
			t.Errorf("Test Scale Vec3 failed. Expected: %v, Got: %v", expect, result)
		}
	}
}

func TestDotProduct(t *testing.T) {
	v1 := Vec2{3.0, 4.0}
	v2 := Vec2{1.0, 2.0}
	vec2Expect := 11.0
	vec2Result := v1.Dot(v2)
	if vec2Expect != vec2Result {
		t.Errorf("Test Dot Product Vec2 failed. Expected: %.2f, Got: %.2f", vec2Expect, vec2Result)
	}
	v3 := Vec3{3.0, 4.0, 5.0}
	v4 := Vec3{1.0, 2.0, 3.0}
	vec3Expect := 26.0
	vec3Result := v3.Dot(v4)
	if vec3Expect != vec3Result {
		t.Errorf("Test Dot Product Vec3 failed. Expected: %.2f, Got: %.2f", vec3Expect, vec3Result)
	}
}

func TestDistance(t *testing.T) {
	v1 := Vec2{8.0, 0.0}
	v2 := Vec2{1.0, 0.0}
	vec2Expect := 7.0
	vec2Result := v1.Distance(v2)
	if vec2Expect != vec2Result {
		t.Errorf("Test Distance Vec2 failed. Expected: %v, Got: %v", vec2Expect, vec2Result)
	}
	v3 := Vec3{10.0, 0.0, 0.0}
	v4 := Vec3{3.0, 0.0, 0.0}
	vec3Expect := 7.0
	vec3Result := v3.Distance(v4)
	if vec3Expect != vec3Result {
		t.Errorf("Test Distance Vec3 failed. Expected: %v, Got: %v", vec3Expect, vec3Result)
	}
}

func TestLerp(t *testing.T) {
	lerpVec2 := testVec2{
		{
			expect: Vec2{2.0, 1.5},
			result: Vec2{1.0, 1.0}.Lerp(0.5, Vec2{3.0, 2.0}),
		},
		{
			expect: Vec2{2.5, 3.0},
			result: Vec2{1.0, 3.0}.Lerp(0.5, Vec2{4.0, 3.0}),
		},
	}
	for _, v := range lerpVec2 {
		expect := v.expect
		result := v.result
		if expect != result {
			t.Errorf("Test Lerp Vec2 failed. Expected: %v, Got: %v", expect, result)
		}
	}
	lerpVec3 := testVec3{
		{
			expect: Vec3{2.0, 3.0, 4.0},
			result: Vec3{3.0, 4.0, 5.0}.Lerp(0.5, Vec3{1.0, 2.0, 3.0}),
		},
		{
			expect: Vec3{11.0, 8.5, 7.0},
			result: Vec3{7.0, 7.0, 7.0}.Lerp(0.5, Vec3{15.0, 10.0, 7.0}),
		},
	}
	for _, v := range lerpVec3 {
		expect := v.expect
		result := v.result
		if expect != result {
			t.Errorf("Test Lerp Vec3 failed. Expected: %v, Got: %v", expect, result)
		}
	}
}

func TestNormalize(t *testing.T) {
	vec2Expect := Vec2{0.6, 0.8}
	vec2Result := Vec2{3.0, 4.0}.Normalize()
	if vec2Expect != vec2Result {
		t.Errorf("Test Normalize Vec2 failed. Expected: %v, Got: %v", vec2Expect, vec2Result)
	}
	vec3Expect := Vec3{0.5883484054145521, 0.7844645405527362, 0.19611613513818404}
	vec3Result := Vec3{3.0, 4.0, 1.0}.Normalize()
	if vec3Expect != vec3Result {
		t.Errorf("Test Normalize Vec3 failed. Expected: %v, Got: %v", vec3Expect, vec3Result)
	}
}

func TestRound(t *testing.T) {
	vec2Expect := Vec2{1.0, 4.0}
	vec2Result := Vec2{0.75, 4.25}.Round()
	if vec2Expect != vec2Result {
		t.Errorf("Test Round Vec2 failed. Expected: %v, Got: %v", vec2Expect, vec2Result)
	}
	vec3Expect := Vec3{1.0, 4.0, 2.0}
	vec3Result := Vec3{0.75, 4.25, 1.5}.Round()
	if vec3Expect != vec3Result {
		t.Errorf("Test Round Vec3 failed. Expected: %v, Got: %v", vec3Expect, vec3Result)
	}
}

func TestStringer(t *testing.T) {
	vec2Expect := "vec2(3.00, 4.00)"
	vec2Result := Vec2{3.0, 4.0}.String()
	if vec2Expect != vec2Result {
		t.Errorf("Test Stringer Vec2 failed. Expected: %v, Got: %v", vec2Expect, vec2Result)
	}
	vec3Expect := "vec3(3.00, 4.00, 1.00)"
	vec3Result := Vec3{3.0, 4.0, 1.0}.String()
	if vec3Expect != vec3Result {
		t.Errorf("Test Stringer Vec3 failed. Expected: %v, Got: %v", vec3Expect, vec3Result)
	}
}
