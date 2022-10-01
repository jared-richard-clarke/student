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
        iter.fold(vec2(0.0, 0.0), |accum, el| Self {
            x: accum.x + el.x,
            y: accum.y + el.y,
        })
    }
}

impl Vector2D {
    pub fn invert(self) -> Self {
        Self {
            x: 1.0 / self.x,
            y: 1.0 / self.y,
        }
    }
    // Returns the magnitude of a vector.
    pub fn mag(self) -> f64 {
        (self.x * self.x + self.y * self.y).sqrt()
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
    #[test]
    fn test_invert() {
        let expect = vec2(0.5, 0.5);
        let result = vec2(2.0, 2.0).invert();
        assert_eq!(result, expect);
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
