use std::iter::Sum;

#[derive(Clone, Copy, PartialEq, Debug, Default)]
struct Vector2D {
    x: f64,
    y: f64,
}

const IHAT: Vector2D = Vector2D { x: 1.0, y: 0.0 };
const JHAT: Vector2D = Vector2D { x: 0.0, y: 1.0 };

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
        Vector2D {
            x: x1 + x2,
            y: y1 + y2,
        }
    }

    fn scale(self, scalar: f64) -> Self {
        let x = self.x;
        let y = self.y;
        Vector2D {
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
        Vector2D {
            x: -self.x,
            y: -self.y,
        }
    }
    fn round(self) -> Self {
        let x = self.x;
        let y = self.y;
        Vector2D {
            x: x.round(),
            y: y.round(),
        }
    }
    fn floor(self) -> Self {
        let x = self.x;
        let y = self.y;
        Vector2D {
            x: x.floor(),
            y: y.floor(),
        }
    }
    fn ceil(self) -> Self {
        let x = self.x;
        let y = self.y;
        Vector2D {
            x: x.ceil(),
            y: y.ceil(),
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
    use crate::Vector2D;
    #[test]
    fn test_default() {
        let v = Vector2D::default();
        assert_eq!(v, Vector2D{x: 0.0, y: 0.0});
    }
    #[test]
    fn test_equal() {
        let v1 = Vector2D { x: 3.0, y: 4.0 };
        let v2 = Vector2D { x: 3.0, y: 4.0 };
        assert_eq!(v1, v2);
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
        let result = Vector2D{x: -3.0, y: 4.0}.flip();
        let expect = Vector2D{x: 3.0, y: -4.0};
        assert_eq!(result, expect);
    }
    #[test]
    fn test_round() {
        let result = Vector2D { x: 0.25, y: 6.73 }.round();
        let expect = Vector2D { x: 0.0, y: 7.0 };
        assert_eq!(result, expect);
    }
    #[test]
    fn test_floor() {
        let result = Vector2D { x: 0.25, y: 6.73 }.floor();
        let expect = Vector2D { x: 0.0, y: 6.0 };
        assert_eq!(result, expect);
    }
    #[test]
    fn test_ceil() {
        let result = Vector2D { x: 0.25, y: 6.73 }.ceil();
        let expect = Vector2D { x: 1.0, y: 7.0 };
        assert_eq!(result, expect);
    }
}
