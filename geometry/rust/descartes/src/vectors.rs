use std::iter::Sum;
use std::ops::{Add, Mul, Neg, Sub};

use crate::matrices::Mat3;

#[derive(Clone, Copy, PartialEq, Debug, Default)]
pub struct Vec2(pub f64, pub f64);

impl Add for Vec2 {
    type Output = Self;
    fn add(self, other: Self) -> Self {
        let Vec2(x1, y1) = self;
        let Vec2(x2, y2) = other;
        Self(x1 + x2, y1 + y2)
    }
}

impl Sub for Vec2 {
    type Output = Self;
    fn sub(self, other: Self) -> Self {
        let Vec2(x1, y1) = self;
        let Vec2(x2, y2) = other;
        Self(x1 - x2, y1 - y2)
    }
}

// lhs: Vec2 * rhs: f64 -> Vec2
impl Mul<f64> for Vec2 {
    type Output = Self;
    fn mul(self, rhs: f64) -> Self {
        let Self(x, y) = self;
        Self(x * rhs, y * rhs)
    }
}

// lhs: f64 * rhs: Vec2 -> Vec2
impl Mul<Vec2> for f64 {
    type Output = Vec2;
    fn mul(self, rhs: Vec2) -> Vec2 {
        let Vec2(x, y) = rhs;
        Vec2(self * x, self * y)
    }
}

impl Neg for Vec2 {
    type Output = Self;
    fn neg(self) -> Self {
        let Self(x, y) = self;
        Self(-x, -y)
    }
}

impl<'a> Sum<&'a Self> for Vec2 {
    fn sum<I>(iter: I) -> Self
    where
        I: Iterator<Item = &'a Self>,
    {
        iter.fold(Vec2(0.0, 0.0), |accum, el| {
            Self(accum.0 + el.0, accum.1 + el.1)
        })
    }
}

impl Vec2 {
    pub fn abs(self) -> Self {
        let Self(x, y) = self;
        Self(x.abs(), y.abs())
    }
    pub fn invert(self) -> Self {
        let Self(x, y) = self;
        Self(1.0 / x, 1.0 / y)
    }
    // Returns the magnitude of a vector.
    pub fn mag(self) -> f64 {
        let Self(x, y) = self;
        (x * x + y * y).sqrt()
    }

    pub fn scale(self, scalar: f64) -> Self {
        self * scalar
    }
    // Dot product.
    pub fn dot(self, other: Self) -> f64 {
        let Self(x1, y1) = self;
        let Self(x2, y2) = other;
        x1 * x2 + y1 * y2
    }
    // Calculates the distance between the tips of two vectors.
    pub fn distance(self, other: Self) -> f64 {
        (self - other).mag()
    }
    // Interpolates two vectors.
    pub fn lerp(self, other: Self, t: f64) -> Self {
        self + (other - self) * t
    }

    pub fn normalize(self) -> Self {
        let mag = self.mag();
        let Self(x, y) = self;
        Self(x / mag, y / mag)
    }

    pub fn round(self) -> Self {
        let Self(x, y) = self;
        Self(x.round(), y.round())
    }

    pub fn transform_by(self, m: Mat3) -> Self {
        let Self(x, y) = self;
        let Mat3(a, b, c, d, _, _) = m;
        Self(a * x + c * y, b * x + d * y)
    }
}

#[derive(Clone, Copy, PartialEq, Debug, Default)]
pub struct Vec3(pub f64, pub f64, pub f64);

impl Add for Vec3 {
    type Output = Self;
    fn add(self, other: Self) -> Self {
        let Self(x1, y1, z1) = self;
        let Self(x2, y2, z2) = other;
        Self(x1 + x2, y1 + y2, z1 + z2)
    }
}

impl Sub for Vec3 {
    type Output = Self;
    fn sub(self, other: Self) -> Self {
        let Self(x1, y1, z1) = self;
        let Self(x2, y2, z2) = other;
        Self(x1 - x2, y1 - y2, z1 - z2)
    }
}
// lhs: Vec3 * rhs: f64 -> Vec3
impl Mul<f64> for Vec3 {
    type Output = Self;
    fn mul(self, rhs: f64) -> Self {
        let Self(x, y, z) = self;
        Self(x * rhs, y * rhs, z * rhs)
    }
}
// lhs: f64 * rhs: Vec3 -> Vec3
impl Mul<Vec3> for f64 {
    type Output = Vec3;
    fn mul(self, rhs: Vec3) -> Vec3 {
        let Vec3(x, y, z) = rhs;
        Vec3(self * x, self * y, self * z)
    }
}

impl Neg for Vec3 {
    type Output = Self;
    fn neg(self) -> Self {
        let Self(x, y, z) = self;
        Self(-x, -y, -z)
    }
}

impl<'a> Sum<&'a Self> for Vec3 {
    fn sum<I>(iter: I) -> Self
    where
        I: Iterator<Item = &'a Self>,
    {
        iter.fold(Vec3(0.0, 0.0, 0.0), |accum, el| {
            Self(accum.0 + el.0, accum.1 + el.1, accum.2 + el.2)
        })
    }
}

impl Vec3 {
    pub fn abs(self) -> Self {
        let Self(x, y, z) = self;
        Self(x.abs(), y.abs(), z.abs())
    }
    pub fn invert(self) -> Self {
        let Self(x, y, z) = self;
        Self(1.0 / x, 1.0 / y, 1.0 / z)
    }
    // Returns the magnitude of a vector.
    pub fn mag(self) -> f64 {
        let Self(x, y, z) = self;
        (x * x + y * y + z * z).sqrt()
    }

    pub fn scale(self, scalar: f64) -> Self {
        self * scalar
    }
    // Dot product.
    pub fn dot(self, other: Self) -> f64 {
        let Self(x1, y1, z1) = self;
        let Self(x2, y2, z2) = other;
        x1 * x2 + y1 * y2 + z1 * z2
    }
    // Calculates the distance between the tips of two vectors.
    pub fn distance(self, other: Self) -> f64 {
        (self - other).mag()
    }
    // Interpolates two vectors.
    pub fn lerp(self, other: Self, t: f64) -> Self {
        self + (other - self) * t
    }

    pub fn normalize(self) -> Self {
        let mag = self.mag();
        let Self(x, y, z) = self;
        Self(x / mag, y / mag, z / mag)
    }

    pub fn round(self) -> Self {
        let Self(x, y, z) = self;
        Self(x.round(), y.round(), z.round())
    }
}

#[cfg(test)]
mod vec2_tests {
    use crate::{matrices::Mat3, vectors::Vec2};
    use std::vec;
    #[test]
    fn test_default() {
        let v = Vec2::default();
        assert_eq!(v, Vec2(0.0, 0.0));
    }
    #[test]
    fn test_equal() {
        let v1 = Vec2(3.0, 4.0);
        let v2 = Vec2(3.0, 4.0);
        assert_eq!(v1, v2);
    }
    #[test]
    fn test_add() {
        let expect = Vec2(5.0, 7.0);
        let result = Vec2(3.0, 4.0) + Vec2(1.0, 2.0) + Vec2(1.0, 1.0);
        assert_eq!(result, expect);
    }
    #[test]
    fn test_sub() {
        let expect = Vec2(2.0, 2.0);
        let result = Vec2(3.0, 4.0) - Vec2(1.0, 2.0);
        assert_eq!(result, expect);
    }
    #[test]
    fn test_mul() {
        let expect = Vec2(6.0, 8.0);
        let result = Vec2(3.0, 4.0) * 2.0;
        assert_eq!(expect, result);
        let result = 2.0 * Vec2(3.0, 4.0);
        assert_eq!(expect, result);
    }
    #[test]
    fn test_neg() {
        let expect = Vec2(3.0, -4.0);
        let result = -Vec2(-3.0, 4.0);
        assert_eq!(result, expect);
    }
    #[test]
    fn test_abs() {
        let expect = Vec2(3.0, 4.0);
        let result = Vec2(-3.0, -4.0).abs();
        assert_eq!(result, expect);
    }
    #[test]
    fn test_invert() {
        let expect = Vec2(0.5, 0.5);
        let result = Vec2(2.0, 2.0).invert();
        assert_eq!(result, expect);
    }
    #[test]
    fn test_sum() {
        let array_expect = Vec2(6.0, 8.0);
        let array_result: Vec2 = [Vec2(1.0, 2.0), Vec2(3.0, 4.0), Vec2(2.0, 2.0)]
            .iter()
            .sum();
        assert_eq!(array_result, array_expect);
        let vector_expect = Vec2(6.0, 8.0);
        let vector_result: Vec2 = vec![Vec2(1.0, 2.0), Vec2(3.0, 4.0), Vec2(2.0, 2.0)]
            .iter()
            .sum();
        assert_eq!(vector_result, vector_expect);
    }
    #[test]
    fn test_mag() {
        let expect = 5.0;
        let result = Vec2(3.0, 4.0).mag();
        assert_eq!(result, expect);
    }
    #[test]
    fn test_scale() {
        let expect = Vec2(6.0, 8.0);
        let result = Vec2(3.0, 4.0).scale(2.0);
        assert_eq!(result, expect);
    }
    #[test]
    fn test_dot() {
        let v1 = Vec2(1.0, 2.0);
        let v2 = Vec2(3.0, 4.0);
        let result = v1.dot(v2);
        assert_eq!(result, 11.0);
    }
    #[test]
    fn test_distance() {
        let expect = 7.0;
        let result = Vec2(8.0, 0.0).distance(Vec2(1.0, 0.0));
        assert_eq!(result, expect);
    }
    #[test]
    fn test_lerp() {
        let expect = Vec2(2.0, 1.5);
        let result = Vec2(1.0, 1.0).lerp(Vec2(3.0, 2.0), 0.5);
        assert_eq!(result, expect);
    }
    #[test]
    fn test_normalize() {
        let expect = Vec2(0.6, 0.8);
        let result = Vec2(3.0, 4.0).normalize();
        assert_eq!(result, expect);
    }
    #[test]
    fn test_round() {
        let expect = Vec2(0.0, 7.0);
        let result = Vec2(0.25, 6.73).round();
        assert_eq!(result, expect);
    }
    #[test]
    fn test_transform_by() {
        let mat = Mat3::identity().scale(10.0, 10.0);
        let expect = Vec2(30.0, 40.0);
        let result = Vec2(3.0, 4.0).transform_by(mat);
        assert_eq!(result, expect);
    }
}

#[cfg(test)]
mod vec3_tests {
    use crate::vectors::Vec3;

    #[test]
    fn test_default() {
        let v = Vec3::default();
        assert_eq!(v, Vec3(0.0, 0.0, 0.0));
    }
    #[test]
    fn test_equal() {
        let v1 = Vec3(3.0, 4.0, 5.0);
        let v2 = Vec3(3.0, 4.0, 5.0);
        assert_eq!(v1, v2);
    }
    #[test]
    fn test_add() {
        let expect = Vec3(5.0, 7.0, 7.0);
        let result = Vec3(3.0, 4.0, 2.0) + Vec3(1.0, 2.0, 2.0) + Vec3(1.0, 1.0, 3.0);
        assert_eq!(result, expect);
    }
    #[test]
    fn test_sub() {
        let expect = Vec3(2.0, 2.0, 2.0);
        let result = Vec3(3.0, 4.0, 5.0) - Vec3(1.0, 2.0, 3.0);
        assert_eq!(result, expect);
    }
    #[test]
    fn test_mul() {
        let expect = Vec3(10.0, 8.0, 6.0);
        let result = Vec3(5.0, 4.0, 3.0) * 2.0;
        assert_eq!(expect, result);
        let result = 2.0 * Vec3(5.0, 4.0, 3.0);
        assert_eq!(expect, result);
    }
    #[test]
    fn test_neg() {
        let expect = Vec3(3.0, -4.0, 5.0);
        let result = -Vec3(-3.0, 4.0, -5.0);
        assert_eq!(result, expect);
    }
    #[test]
    fn test_abs() {
        let expect = Vec3(3.0, 4.0, 5.0);
        let result = Vec3(-3.0, 4.0, -5.0).abs();
        assert_eq!(result, expect);
    }
    #[test]
    fn test_invert() {
        let expect = Vec3(0.5, 0.5, 0.25);
        let result = Vec3(2.0, 2.0, 4.0).invert();
        assert_eq!(result, expect);
    }
    #[test]
    fn test_sum() {
        let array_expect = Vec3(6.0, 8.0, 3.0);
        let array_result: Vec3 = [
            Vec3(1.0, 2.0, 1.0),
            Vec3(3.0, 4.0, 1.0),
            Vec3(2.0, 2.0, 1.0),
        ]
        .iter()
        .sum();
        assert_eq!(array_result, array_expect);
        let vector_expect = Vec3(6.0, 8.0, 15.0);
        let vector_result: Vec3 = vec![
            Vec3(1.0, 2.0, 5.0),
            Vec3(3.0, 4.0, 5.0),
            Vec3(2.0, 2.0, 5.0),
        ]
        .iter()
        .sum();
        assert_eq!(vector_result, vector_expect);
    }
    #[test]
    fn test_mag() {
        let expect = 7.0710678118654755;
        let result = Vec3(3.0, 4.0, 5.0).mag();
        assert_eq!(result, expect);
    }
    #[test]
    fn test_scale() {
        let expect = Vec3(6.0, 8.0, 10.0);
        let result = Vec3(3.0, 4.0, 5.0).scale(2.0);
        assert_eq!(result, expect);
    }
    #[test]
    fn test_dot() {
        let v1 = Vec3(1.0, 2.0, 3.0);
        let v2 = Vec3(3.0, 4.0, 5.0);
        let result = v1.dot(v2);
        assert_eq!(result, 26.0);
    }
    #[test]
    fn test_distance() {
        let expect = 7.0;
        let result = Vec3(8.0, 0.0, 0.0).distance(Vec3(1.0, 0.0, 0.0));
        assert_eq!(result, expect);
    }
    #[test]
    fn test_lerp() {
        let expect = Vec3(2.0, 1.5, 1.0);
        let result = Vec3(1.0, 1.0, 1.0).lerp(Vec3(3.0, 2.0, 1.0), 0.5);
        assert_eq!(result, expect);
    }
    #[test]
    fn test_normalize() {
        let expect = Vec3(0.4242640687119285, 0.565685424949238, 0.7071067811865475);
        let result = Vec3(3.0, 4.0, 5.0).normalize();
        assert_eq!(result, expect);
    }
    #[test]
    fn test_round() {
        let result = Vec3(0.25, 6.73, 0.5).round();
        let expect = Vec3(0.0, 7.0, 1.0);
        assert_eq!(result, expect);
    }
}

