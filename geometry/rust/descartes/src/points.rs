use crate::vectors::{Vector2D, Vector3D};
use std::ops::Sub;

#[derive(Clone, Copy, PartialEq, Debug, Default)]
pub struct Point2D {
    pub x: f64,
    pub y: f64,
}

pub fn pt2(x: f64, y: f64) -> Point2D {
    Point2D { x, y }
}

impl Sub for Point2D {
    type Output = Vector2D;
    fn sub(self, other: Self) -> Vector2D {
        Vector2D {
            x: self.x - other.x,
            y: self.y - other.y,
        }
    }
}

impl Point2D {
    pub fn distance(self, other: Self) -> f64 {
        (other.x - self.x).hypot(other.y - self.y)
    }
    pub fn lerp(self, other: Self, t: f64) -> Self {
        Self {
            x: self.x + (other.x - self.x) * t,
            y: self.y + (other.y - self.y) * t,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Debug, Default)]
pub struct Point3D {
    pub x: f64,
    pub y: f64,
    pub z: f64,
}

pub fn pt3(x: f64, y: f64, z: f64) -> Point3D {
    Point3D { x, y, z }
}

impl Sub for Point3D {
    type Output = Vector3D;
    fn sub(self, other: Self) -> Vector3D {
        Vector3D {
            x: self.x - other.x,
            y: self.y - other.y,
            z: self.z - other.z,
        }
    }
}

impl Point3D {
    pub fn distance(self, other: Self) -> f64 {
        (other - self).mag()
    }
    pub fn lerp(self, other: Self, t: f64) -> Self {
        Self {
            x: self.x + (other.x - self.x) * t,
            y: self.y + (other.y - self.y) * t,
            z: self.z + (other.z - self.z) * t,
        }
    }
}

#[cfg(test)]
mod pt2_tests {
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
        let result = pt2(10.0, 7.0) - pt2(5.0, 6.0);
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
