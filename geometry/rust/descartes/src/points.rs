use crate::vectors::{Vector2D, Vector3D};
use std::ops::{Add, Sub};

#[derive(Clone, Copy, PartialEq, Debug, Default)]
pub struct Point2D {
    pub x: f64,
    pub y: f64,
}

pub fn pt2(x: f64, y: f64) -> Point2D {
    Point2D { x, y }
}

// lhs: Point2D + rhs: Vector2D -> Point2D
impl Add<Vector2D> for Point2D {
    type Output = Self;
    fn add(self, rhs: Vector2D) -> Self {
        Self {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}
// lhs: Vector2D + rhs: Point2D -> Point2D
impl Add<Point2D> for Vector2D {
    type Output = Point2D;
    fn add(self, rhs: Point2D) -> Self::Output {
        Self::Output {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}
// lhs: Point2D - rhs: Point2d -> Vector2D
impl Sub for Point2D {
    type Output = Vector2D;
    fn sub(self, rhs: Self) -> Self::Output {
        Self::Output {
            x: self.x - rhs.x,
            y: self.y - rhs.y,
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

// lhs: Point3D + rhs: Vector3D -> Point3D
impl Add<Vector3D> for Point3D {
    type Output = Self;
    fn add(self, rhs: Vector3D) -> Self {
        Self {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
            z: self.z + rhs.z,
        }
    }
}
// lhs: Vector3D + rhs: Point3D -> Point3D
impl Add<Point3D> for Vector3D {
    type Output = Point3D;
    fn add(self, rhs: Point3D) -> Self::Output {
        Self::Output {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
            z: self.z + rhs.z,
        }
    }
}
// lhs: Point3D - rhs: Point3D -> Vector3D
impl Sub for Point3D {
    type Output = Vector3D;
    fn sub(self, other: Self) -> Self::Output {
        Self::Output {
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
    fn test_add() {
        let expect = pt2(4.0, 6.0);
        let result = pt2(3.0, 4.0) + vec2(1.0, 2.0);
        assert_eq!(expect, result);
        let result = vec2(1.0, 2.0) + pt2(3.0, 4.0);
        assert_eq!(expect, result);
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

#[cfg(test)]
mod vec3_tests {
    use crate::{
        points::{pt3, Point3D},
        vectors::vec3,
    };
    #[test]
    fn test_default() {
        let p = Point3D::default();
        assert_eq!(p, pt3(0.0, 0.0, 0.0));
    }
    #[test]
    fn test_equal() {
        let p1 = pt3(3.0, 4.0, 5.0);
        let p2 = pt3(3.0, 4.0, 5.0);
        assert!(p1 == p2);
    }
    #[test]
    fn test_add() {
        let expect = pt3(10.0, 9.0, 8.0);
        let result = pt3(5.0, 4.5, 4.0) + vec3(5.0, 4.5, 4.0);
        assert_eq!(expect, result);
        let result = vec3(5.0, 4.5, 4.0) + pt3(5.0, 4.5, 4.0);
        assert_eq!(expect, result);
    }
    #[test]
    fn test_sub() {
        let expect = vec3(5.0, 1.0, 0.0);
        let result = pt3(10.0, 7.0, 5.0) - pt3(5.0, 6.0, 5.0);
        assert_eq!(expect, result);
    }
    #[test]
    fn test_distance() {
        let expect = 5.0;
        let result = pt3(10.0, 0.0, 0.0).distance(pt3(5.0, 0.0, 0.0));
        assert_eq!(expect, result);
    }
    #[test]
    fn test_lerp() {
        let expect = pt3(5.0, 0.0, 0.0);
        let result = pt3(0.0, 0.0, 0.0).lerp(pt3(10.0, 0.0, 0.0), 0.5);
        assert_eq!(expect, result);
    }
}
