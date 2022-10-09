use std::iter::Sum;
use std::ops::{Add, Mul, Neg, Sub};

#[derive(Clone, Copy, PartialEq, Debug, Default)]
pub struct Vector2D {
    pub x: f64,
    pub y: f64,
}

pub fn vec2(x: f64, y: f64) -> Vector2D {
    Vector2D { x, y }
}

impl Add for Vector2D {
    type Output = Self;
    fn add(self, other: Self) -> Self {
        Self {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

impl Sub for Vector2D {
    type Output = Self;
    fn sub(self, other: Self) -> Self {
        Self {
            x: self.x - other.x,
            y: self.y - other.y,
        }
    }
}
// lhs: Vector2D * rhs: f64 -> Vector2D
impl Mul<f64> for Vector2D {
    type Output = Self;
    fn mul(self, rhs: f64) -> Self {
        Self {
            x: self.x * rhs,
            y: self.y * rhs,
        }
    }
}
// lhs: f64 * rhs: Vector2D -> Vector2D
impl Mul<Vector2D> for f64 {
    type Output = Vector2D;
    fn mul(self, rhs: Vector2D) -> Self::Output {
        Self::Output {
            x: self * rhs.x,
            y: self * rhs.y,
        }
    }
}

impl Neg for Vector2D {
    type Output = Self;
    fn neg(self) -> Self {
        Self {
            x: -self.x,
            y: -self.y,
        }
    }
}

impl<'a> Sum<&'a Self> for Vector2D {
    fn sum<I>(iter: I) -> Self
    where
        I: Iterator<Item = &'a Self>,
    {
        iter.fold(vec2(0.0, 0.0), |accum, el| Self {
            x: accum.x + el.x,
            y: accum.y + el.y,
        })
    }
}

impl Vector2D {
    pub fn abs(self) -> Self {
        Self {
            x: self.x.abs(),
            y: self.y.abs(),
        }
    }
    pub fn invert(self) -> Self {
        Self {
            x: 1.0 / self.x,
            y: 1.0 / self.y,
        }
    }
    // Returns the magnitude of a vector.
    pub fn mag(self) -> f64 {
        (self.x.powi(2) + self.y.powi(2)).sqrt()
    }

    pub fn scale(self, scalar: f64) -> Self {
        Self {
            x: self.x * scalar,
            y: self.y * scalar,
        }
    }
    // Dot product.
    pub fn dot(self, other: Self) -> f64 {
        self.x * other.x + self.y * other.y
    }
    // Calculates the distance between the tips of two vectors.
    pub fn distance(self, other: Self) -> f64 {
        (self - other).mag()
    }
    // Interpolates two vectors.
    pub fn lerp(self, other: Self, t: f64) -> Self {
        self + (other - self).scale(t)
    }

    pub fn normalize(self) -> Self {
        let mag = self.mag();
        Self {
            x: self.x / mag,
            y: self.y / mag,
        }
    }

    pub fn round(self) -> Self {
        Self {
            x: self.x.round(),
            y: self.y.round(),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Debug, Default)]
pub struct Vector3D {
    pub x: f64,
    pub y: f64,
    pub z: f64,
}

pub fn vec3(x: f64, y: f64, z: f64) -> Vector3D {
    Vector3D { x, y, z }
}

impl Add for Vector3D {
    type Output = Self;
    fn add(self, other: Self) -> Self {
        Self {
            x: self.x + other.x,
            y: self.y + other.y,
            z: self.z + other.z,
        }
    }
}

impl Sub for Vector3D {
    type Output = Self;
    fn sub(self, other: Self) -> Self {
        Self {
            x: self.x - other.x,
            y: self.y - other.y,
            z: self.z - other.z,
        }
    }
}
// lhs: Vector3D * rhs: f64 -> Vector3D
impl Mul<f64> for Vector3D {
    type Output = Self;
    fn mul(self, rhs: f64) -> Self {
        Self {
            x: self.x * rhs,
            y: self.y * rhs,
            z: self.z * rhs,
        }
    }
}
// lhs: f64 * rhs: Vector3D -> Vector3D
impl Mul<Vector3D> for f64 {
    type Output = Vector3D;
    fn mul(self, rhs: Vector3D) -> Self::Output {
        Self::Output {
            x: self * rhs.x,
            y: self * rhs.y,
            z: self * rhs.z,
        }
    }
}

impl Neg for Vector3D {
    type Output = Self;
    fn neg(self) -> Self {
        Self {
            x: -self.x,
            y: -self.y,
            z: -self.z,
        }
    }
}

impl<'a> Sum<&'a Self> for Vector3D {
    fn sum<I>(iter: I) -> Self
    where
        I: Iterator<Item = &'a Self>,
    {
        iter.fold(vec3(0.0, 0.0, 0.0), |accum, el| Self {
            x: accum.x + el.x,
            y: accum.y + el.y,
            z: accum.z + el.z,
        })
    }
}

impl Vector3D {
    pub fn abs(self) -> Self {
        Self {
            x: self.x.abs(),
            y: self.y.abs(),
            z: self.z.abs(),
        }
    }
    pub fn invert(self) -> Self {
        Self {
            x: 1.0 / self.x,
            y: 1.0 / self.y,
            z: 1.0 / self.z,
        }
    }
    // Returns the magnitude of a vector.
    pub fn mag(self) -> f64 {
        (self.x.powi(2) + self.y.powi(2) + self.z.powi(2)).sqrt()
    }

    pub fn scale(self, scalar: f64) -> Self {
        Self {
            x: self.x * scalar,
            y: self.y * scalar,
            z: self.z * scalar,
        }
    }
    // Dot product.
    pub fn dot(self, other: Self) -> f64 {
        self.x * other.x + self.y * other.y + self.z * other.z
    }
    // Calculates the distance between the tips of two vectors.
    pub fn distance(self, other: Self) -> f64 {
        (self - other).mag()
    }
    // Interpolates two vectors.
    pub fn lerp(self, other: Self, t: f64) -> Self {
        self + (other - self).scale(t)
    }

    pub fn normalize(self) -> Self {
        let mag = self.mag();
        Self {
            x: self.x / mag,
            y: self.y / mag,
            z: self.z / mag,
        }
    }

    pub fn round(self) -> Self {
        Self {
            x: self.x.round(),
            y: self.y.round(),
            z: self.z.round(),
        }
    }
}

#[cfg(test)]
mod vec2_tests {
    use crate::vectors::{vec2, Vector2D};
    use std::vec;
    #[test]
    fn test_default() {
        let v = Vector2D::default();
        assert_eq!(v, vec2(0.0, 0.0));
    }
    #[test]
    fn test_equal() {
        let v1 = vec2(3.0, 4.0);
        let v2 = vec2(3.0, 4.0);
        assert_eq!(v1, v2);
    }
    #[test]
    fn test_vec2() {
        let expect = Vector2D { x: 3.0, y: 4.0 };
        let result = vec2(3.0, 4.0);
        assert_eq!(result, expect);
    }
    #[test]
    fn test_add() {
        let expect = vec2(5.0, 7.0);
        let result = vec2(3.0, 4.0) + vec2(1.0, 2.0) + vec2(1.0, 1.0);
        assert_eq!(result, expect);
    }
    #[test]
    fn test_sub() {
        let expect = vec2(2.0, 2.0);
        let result = vec2(3.0, 4.0) - vec2(1.0, 2.0);
        assert_eq!(result, expect);
    }
    #[test]
    fn test_mul() {
        let expect = vec2(6.0, 8.0);
        let result = vec2(3.0, 4.0) * 2_f64;
        assert_eq!(expect, result);
        let result = 2_f64 * vec2(3.0, 4.0);
        assert_eq!(expect, result);
    }
    #[test]
    fn test_neg() {
        let expect = vec2(3.0, -4.0);
        let result = -vec2(-3.0, 4.0);
        assert_eq!(result, expect);
    }
    #[test]
    fn test_abs() {
        let expect = vec2(3.0, 4.0);
        let result = vec2(-3.0, -4.0).abs();
        assert_eq!(result, expect);
    }
    #[test]
    fn test_invert() {
        let expect = vec2(0.5, 0.5);
        let result = vec2(2.0, 2.0).invert();
        assert_eq!(result, expect);
    }
    #[test]
    fn test_sum() {
        let array_expect = vec2(6.0, 8.0);
        let array_result: Vector2D = [vec2(1.0, 2.0), vec2(3.0, 4.0), vec2(2.0, 2.0)]
            .iter()
            .sum();
        assert_eq!(array_result, array_expect);
        let vector_expect = vec2(6.0, 8.0);
        let vector_result: Vector2D = vec![vec2(1.0, 2.0), vec2(3.0, 4.0), vec2(2.0, 2.0)]
            .iter()
            .sum();
        assert_eq!(vector_result, vector_expect);
    }
    #[test]
    fn test_mag() {
        let expect = 5.0;
        let result = vec2(3.0, 4.0).mag();
        assert_eq!(result, expect);
    }
    #[test]
    fn test_scale() {
        let expect = vec2(6.0, 8.0);
        let result = vec2(3.0, 4.0).scale(2.0);
        assert_eq!(result, expect);
    }
    #[test]
    fn test_dot() {
        let v1 = vec2(1.0, 2.0);
        let v2 = vec2(3.0, 4.0);
        let result = v1.dot(v2);
        assert_eq!(result, 11.0);
    }
    #[test]
    fn test_distance() {
        let expect = 7.0;
        let result = vec2(8.0, 0.0).distance(vec2(1.0, 0.0));
        assert_eq!(result, expect);
    }
    #[test]
    fn test_lerp() {
        let expect = vec2(2.0, 1.5);
        let result = vec2(1.0, 1.0).lerp(vec2(3.0, 2.0), 0.5);
        assert_eq!(result, expect);
    }
    #[test]
    fn test_normalize() {
        let expect = vec2(0.6, 0.8);
        let result = vec2(3.0, 4.0).normalize();
        assert_eq!(result, expect);
    }
    #[test]
    fn test_round() {
        let result = vec2(0.25, 6.73).round();
        let expect = vec2(0.0, 7.0);
        assert_eq!(result, expect);
    }
}

#[cfg(test)]
mod vec3_tests {
    use crate::vectors::{vec3, Vector3D};

    #[test]
    fn test_default() {
        let v = Vector3D::default();
        assert_eq!(v, vec3(0.0, 0.0, 0.0));
    }
    #[test]
    fn test_equal() {
        let v1 = vec3(3.0, 4.0, 5.0);
        let v2 = vec3(3.0, 4.0, 5.0);
        assert_eq!(v1, v2);
    }
    #[test]
    fn test_vec3() {
        let expect = Vector3D {
            x: 3.0,
            y: 4.0,
            z: 5.0,
        };
        let result = vec3(3.0, 4.0, 5.0);
        assert_eq!(result, expect);
    }
    #[test]
    fn test_add() {
        let expect = vec3(5.0, 7.0, 7.0);
        let result = vec3(3.0, 4.0, 2.0) + vec3(1.0, 2.0, 2.0) + vec3(1.0, 1.0, 3.0);
        assert_eq!(result, expect);
    }
    #[test]
    fn test_sub() {
        let expect = vec3(2.0, 2.0, 2.0);
        let result = vec3(3.0, 4.0, 5.0) - vec3(1.0, 2.0, 3.0);
        assert_eq!(result, expect);
    }
    #[test]
    fn test_mul() {
        let expect = vec3(10.0, 8.0, 6.0);
        let result = vec3(5.0, 4.0, 3.0) * 2_f64;
        assert_eq!(expect, result);
        let result = 2_f64 * vec3(5.0, 4.0, 3.0);
        assert_eq!(expect, result);
    }
    #[test]
    fn test_neg() {
        let expect = vec3(3.0, -4.0, 5.0);
        let result = -vec3(-3.0, 4.0, -5.0);
        assert_eq!(result, expect);
    }
    #[test]
    fn test_abs() {
        let expect = vec3(3.0, 4.0, 5.0);
        let result = vec3(-3.0, 4.0, -5.0).abs();
        assert_eq!(result, expect);
    }
    #[test]
    fn test_invert() {
        let expect = vec3(0.5, 0.5, 0.25);
        let result = vec3(2.0, 2.0, 4.0).invert();
        assert_eq!(result, expect);
    }
    #[test]
    fn test_sum() {
        let array_expect = vec3(6.0, 8.0, 3.0);
        let array_result: Vector3D = [
            vec3(1.0, 2.0, 1.0),
            vec3(3.0, 4.0, 1.0),
            vec3(2.0, 2.0, 1.0),
        ]
        .iter()
        .sum();
        assert_eq!(array_result, array_expect);
        let vector_expect = vec3(6.0, 8.0, 15.0);
        let vector_result: Vector3D = vec![
            vec3(1.0, 2.0, 5.0),
            vec3(3.0, 4.0, 5.0),
            vec3(2.0, 2.0, 5.0),
        ]
        .iter()
        .sum();
        assert_eq!(vector_result, vector_expect);
    }
    #[test]
    fn test_mag() {
        let expect = 7.0710678118654755;
        let result = vec3(3.0, 4.0, 5.0).mag();
        assert_eq!(result, expect);
    }
    #[test]
    fn test_scale() {
        let expect = vec3(6.0, 8.0, 10.0);
        let result = vec3(3.0, 4.0, 5.0).scale(2.0);
        assert_eq!(result, expect);
    }
    #[test]
    fn test_dot() {
        let v1 = vec3(1.0, 2.0, 3.0);
        let v2 = vec3(3.0, 4.0, 5.0);
        let result = v1.dot(v2);
        assert_eq!(result, 26.0);
    }
    #[test]
    fn test_distance() {
        let expect = 7.0;
        let result = vec3(8.0, 0.0, 0.0).distance(vec3(1.0, 0.0, 0.0));
        assert_eq!(result, expect);
    }
    #[test]
    fn test_lerp() {
        let expect = vec3(2.0, 1.5, 1.0);
        let result = vec3(1.0, 1.0, 1.0).lerp(vec3(3.0, 2.0, 1.0), 0.5);
        assert_eq!(result, expect);
    }
    #[test]
    fn test_normalize() {
        let expect = vec3(0.4242640687119285, 0.565685424949238, 0.7071067811865475);
        let result = vec3(3.0, 4.0, 5.0).normalize();
        assert_eq!(result, expect);
    }
    #[test]
    fn test_round() {
        let result = vec3(0.25, 6.73, 0.5).round();
        let expect = vec3(0.0, 7.0, 1.0);
        assert_eq!(result, expect);
    }
}
