use std::iter::Sum;
use std::ops::{Add, Neg, Sub};

#[derive(Clone, Copy, PartialEq, Debug, Default)]
pub struct Vector2D {
    pub x: f64,
    pub y: f64,
}

pub fn vec2(x: f64, y: f64) -> Vector2D {
    Vector2D { x, y }
}

impl Vector2D {
    pub fn length(self) -> f64 {
        let x = self.x;
        let y = self.y;
        (x * x + y * y).sqrt()
    }

    pub fn scale(self, scalar: f64) -> Self {
        let x = self.x;
        let y = self.y;
        Self {
            x: x * scalar,
            y: y * scalar,
        }
    }
    // Dot product.
    pub fn dot(self, other: Self) -> f64 {
        let x1 = self.x;
        let y1 = self.y;
        let x2 = other.x;
        let y2 = other.y;
        x1 * x2 + y1 * y2
    }
    // Calculates the distance between two vector points.
    pub fn distance(self, other: Self) -> f64 {
        (self - other).length()
    }
    // Interpolates point between two vector points.
    pub fn lerp(self, other: Self, t: f64) -> Self {
        let x = self.x + (other.x - self.x) * t;
        let y = self.y + (other.y - self.y) * t;
        Self { x, y }
    }

    pub fn round(self) -> Self {
        let x = self.x;
        let y = self.y;
        Self {
            x: x.round(),
            y: y.round(),
        }
    }

    pub fn normalize(self) -> Self {
        let mag = self.length();
        let x = self.x;
        let y = self.y;
        Self {
            x: x / mag,
            y: y / mag,
        }
    }
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
        iter.fold(Vector2D { x: 0.0, y: 0.0 }, |accum, el| Self {
            x: accum.x + el.x,
            y: accum.y + el.y,
        })
    }
}

#[cfg(test)]
mod tests {
    use std::vec;

    use crate::vectors::{vec2, Vector2D};
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
    fn test_length() {
        let expect = 5.0;
        let result = vec2(3.0, 4.0).length();
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
        let expect = vec2(-8.0, 24.0);
        let result = vec2(0.0, 10.0).lerp(vec2(8.0, -4.0), -1.0);
        assert_eq!(result, expect);
    }
    #[test]
    fn test_round() {
        let result = vec2(0.25, 6.73).round();
        let expect = vec2(0.0, 7.0);
        assert_eq!(result, expect);
    }
    #[test]
    fn test_normalize() {
        let expect = vec2(0.6, 0.8);
        let result = vec2(3.0, 4.0).normalize();
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
    fn test_neg() {
        let expect = vec2(3.0, -4.0);
        let result = -vec2(-3.0, 4.0);
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
}
