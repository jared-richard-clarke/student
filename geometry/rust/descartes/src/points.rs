use crate::vectors::Vector2D;

#[derive(Clone, Copy, PartialEq, Debug, Default)]
// A cartesian representation of a point in two dimensions.
pub struct Point2D {
    pub x: f64,
    pub y: f64,
}

pub fn pt2(x: f64, y: f64) -> Point2D {
    Point2D { x, y }
}

impl Point2D {
    // Returns a vector by subtracting two points.
    pub fn sub(self, other: Self) -> Vector2D {
        Vector2D {
            x: self.x - other.x,
            y: self.y - other.y,
        }
    }
    // Returns the distance between two points.
    pub fn distance(self, other: Self) -> f64 {
        let x1 = self.x;
        let y1 = self.y;
        let x2 = other.x;
        let y2 = other.y;
        (x2 - x1).hypot(y2 - y1)
    }
    // Interpolates a point along a line between two points.
    pub fn lerp(self, other: Self, t: f64) -> Self {
        let x1 = self.x;
        let y1 = self.y;
        let x2 = other.x;
        let y2 = other.y;
        let x = x1 + (x2 - x1) * t;
        let y = y1 + (y2 - y1) * t;
        Self { x, y }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        points::{pt2, Point2D},
        vectors::vec2,
    };

    #[test]
    fn test_default() {
        let p = Point2D::default();
        assert_eq!(p, pt2(0.0, 0.0));
    }
    #[test]
    fn test_equal() {
        let p1 = pt2(3.0, 4.0);
        let p2 = pt2(3.0, 4.0);
        assert!(p1 == p2);
    }
    #[test]
    fn test_sub() {
        let expect = vec2(5.0, 1.0);
        let result = pt2(10.0, 7.0).sub(pt2(5.0, 6.0));
        assert_eq!(expect, result);
    }
    #[test]
    fn test_distance() {
        let expect = 5.0;
        let result = pt2(10.0, 0.0).distance(pt2(5.0, 0.0));
        assert_eq!(expect, result);
    }
    #[test]
    fn test_lerp() {
        let expect = pt2(5.0, 0.0);
        let result = pt2(0.0, 0.0).lerp(pt2(10.0, 0.0), 0.5);
        assert_eq!(expect, result);
    }
}
