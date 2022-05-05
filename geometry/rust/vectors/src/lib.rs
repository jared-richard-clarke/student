use core::iter::Sum;
use std::fmt;

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
}

impl<'a> Sum<&'a Self> for Vector2D {
    fn sum<I>(iter: I) -> Self
    where
        I: Iterator<Item = &'a Self>,
    {
        iter.fold(Vector2D { x: 0.0, y: 0.0 }, |v1, v2| Vector2D {
            x: v1.x + v2.x,
            y: v1.y + v2.y,
        })
    }
}

impl Copy for Vector2D {}

impl Clone for Vector2D {
    fn clone(&self) -> Self {
        Vector2D {
            x: self.x.clone(),
            y: self.y.clone(),
        }
    }
}

impl Eq for Vector2D {}

impl PartialEq for Vector2D {
    fn eq(&self, other: &Self) -> bool {
        self.x == other.x && self.y == other.y
    }
}
impl fmt::Debug for Vector2D {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_tuple("").field(&self.x).field(&self.y).finish()
    }
}

#[cfg(test)]
mod tests {
    use crate::Vector2D;

    #[test]
    fn test_sum() {
        let vs = [
            Vector2D { x: 1.0, y: 2.0 },
            Vector2D { x: 1.0, y: 2.0 },
            Vector2D { x: 1.0, y: 2.0 },
        ];
        let result: Vector2D = vs.iter().sum();
        assert_eq!(result, Vector2D { x: 3.0, y: 6.0 });
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
}
