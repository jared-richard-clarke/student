use std::iter::Sum;
use std::ops::Add;

#[derive(Clone, Copy, PartialEq, Debug, Default)]
struct Vector2D {
    x: f64,
    y: f64,
}

const IHAT: Vector2D = Vector2D { x: 1.0, y: 0.0 };
const JHAT: Vector2D = Vector2D { x: 0.0, y: 1.0 };

fn vec2(x: f64, y: f64) -> Vector2D {
    Vector2D { x, y }
}

impl Vector2D {
    fn magnitude(self) -> f64 {
        let x = self.x;
        let y = self.y;
        (x * x + y * y).sqrt()
    }

    fn add(self, other: Self) -> Self {
        let x1 = self.x;
        let y1 = self.y;
        let x2 = other.x;
        let y2 = other.y;
        Self {
            x: x1 + x2,
            y: y1 + y2,
        }
    }

    fn scale(self, scalar: f64) -> Self {
        let x = self.x;
        let y = self.y;
        Self {
            x: x * scalar,
            y: y * scalar,
        }
    }
    // Dot product.
    fn dot(self, other: Self) -> f64 {
        let x1 = self.x;
        let y1 = self.y;
        let x2 = other.x;
        let y2 = other.y;
        x1 * x2 + y1 * y2
    }
    // Cross product.
    fn cross(self, other: Self) -> f64 {
        let x1 = self.x;
        let y1 = self.y;
        let x2 = other.x;
        let y2 = other.y;
        x1 * y2 - y1 * x2
    }

    fn flip(self) -> Self {
        Self {
            x: -self.x,
            y: -self.y,
        }
    }

    fn round(self) -> Self {
        let x = self.x;
        let y = self.y;
        Self {
            x: x.round(),
            y: y.round(),
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

impl<'a> Sum<&'a Self> for Vector2D {
    fn sum<I>(iter: I) -> Self
    where
        I: Iterator<Item = &'a Self>,
    {
        iter.fold(Vector2D { x: 0.0, y: 0.0 }, |accum, el| Vector2D {
            x: accum.x + el.x,
            y: accum.y + el.y,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::{vec2, Vector2D};
    #[test]
    fn test_default() {
        let v = Vector2D::default();
        assert_eq!(v, Vector2D { x: 0.0, y: 0.0 });
    }
    #[test]
    fn test_equal() {
        let v1 = Vector2D { x: 3.0, y: 4.0 };
        let v2 = Vector2D { x: 3.0, y: 4.0 };
        assert_eq!(v1, v2);
    }
    #[test]
    fn test_vec2() {
        let expect = Vector2D { x: 3.0, y: 4.0 };
        let result = vec2(3.0, 4.0);
        assert_eq!(result, expect);
    }
    #[test]
    fn test_magnitude() {
        let v = Vector2D { x: 3.0, y: 4.0 };
        let result = v.magnitude();
        assert_eq!(result, 5.0);
    }
    #[test]
    fn test_add() {
        let v1 = Vector2D { x: 1.0, y: 2.0 };
        let v2 = Vector2D { x: 3.0, y: 4.0 };
        let result = v1.add(v2);
        assert_eq!(result, Vector2D { x: 4.0, y: 6.0 });
    }
    #[test]
    fn test_scale() {
        let v = Vector2D { x: 3.0, y: 4.0 };
        let result = v.scale(2.0);
        assert_eq!(result, Vector2D { x: 6.0, y: 8.0 });
    }
    #[test]
    fn test_dot() {
        let v1 = Vector2D { x: 1.0, y: 2.0 };
        let v2 = Vector2D { x: 3.0, y: 4.0 };
        let result = v1.dot(v2);
        assert_eq!(result, 11.0);
    }
    #[test]
    fn test_cross() {
        let v1 = Vector2D { x: 1.0, y: 2.0 };
        let v2 = Vector2D { x: 3.0, y: 4.0 };
        let result = v1.cross(v2);
        assert_eq!(result, -2.0);
    }
    #[test]
    fn test_flip() {
        let result = Vector2D { x: -3.0, y: 4.0 }.flip();
        let expect = Vector2D { x: 3.0, y: -4.0 };
        assert_eq!(result, expect);
    }
    #[test]
    fn test_round() {
        let result = Vector2D { x: 0.25, y: 6.73 }.round();
        let expect = Vector2D { x: 0.0, y: 7.0 };
        assert_eq!(result, expect);
    }
    #[test]
    fn test_sum() {
        let vs = [
            Vector2D { x: 1.0, y: 2.0 },
            Vector2D { x: 3.0, y: 4.0 },
            Vector2D { x: 2.0, y: 2.0 },
        ];
        let result: Vector2D = vs.iter().sum();
        assert_eq!(result, Vector2D { x: 6.0, y: 8.0 });
    }
    #[test]
    fn test_add_overload() {
        let expect = Vector2D { x: 4.0, y: 6.0 };
        let result = Vector2D { x: 3.0, y: 4.0 } + Vector2D { x: 1.0, y: 2.0 };
        assert_eq!(result, expect);
    }
}
