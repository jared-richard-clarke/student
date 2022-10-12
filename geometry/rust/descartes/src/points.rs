use crate::{
    matrices::Mat3,
    vectors::{Vec2, Vec3},
};
use std::ops::{Add, Sub};

#[derive(Clone, Copy, PartialEq, Debug, Default)]
pub struct Pt2(pub f64, pub f64);

// lhs: Pt2 + rhs: Vec2 -> Pt2
impl Add<Vec2> for Pt2 {
    type Output = Self;
    fn add(self, rhs: Vec2) -> Self {
        let Self(x1, y1) = self;
        let Vec2(x2, y2) = rhs;
        Self(x1 + x2, y1 + y2)
    }
}
// lhs: Vec2 + rhs: Pt2 -> Pt2
impl Add<Pt2> for Vec2 {
    type Output = Pt2;
    fn add(self, rhs: Pt2) -> Pt2 {
        let Self(x1, y1) = self;
        let Pt2(x2, y2) = rhs;
        Pt2(x1 + x2, y1 + y2)
    }
}
// lhs: Pt2 - rhs: Pt2 -> Vec2
impl Sub for Pt2 {
    type Output = Vec2;
    fn sub(self, rhs: Self) -> Vec2 {
        let Self(x1, y1) = self;
        let Self(x2, y2) = rhs;
        Vec2(x1 - x2, y1 - y2)
    }
}

impl Pt2 {
    pub fn distance(self, other: Self) -> f64 {
        let Self(x1, y1) = self;
        let Self(x2, y2) = other;
        (x2 - x1).hypot(y2 - y1)
    }
    pub fn lerp(self, other: Self, t: f64) -> Self {
        let Self(x1, y1) = self;
        let Self(x2, y2) = other;
        Self(x1 + (x2 - x1) * t, y1 + (y2 - y1) * t)
    }
    pub fn transform_by(self, m: Mat3) -> Self {
        let Self(x, y) = self;
        let Mat3(a, b, c, d, e, f) = m;
        Self(a * x + c * y + e, b * x + d * y + f)
    }
}

#[derive(Clone, Copy, PartialEq, Debug, Default)]
pub struct Pt3(pub f64, pub f64, pub f64);

// lhs: Pt3 + rhs: Vec3 -> Pt3
impl Add<Vec3> for Pt3 {
    type Output = Self;
    fn add(self, rhs: Vec3) -> Self {
        let Self(x1, y1, z1) = self;
        let Vec3(x2, y2, z2) = rhs;
        Self(x1 + x2, y1 + y2, z1 + z2)
    }
}
// lhs: Vec3 + rhs: Pt3 -> Pt3
impl Add<Pt3> for Vec3 {
    type Output = Pt3;
    fn add(self, rhs: Pt3) -> Pt3 {
        let Self(x1, y1, z1) = self;
        let Pt3(x2, y2, z2) = rhs;
        Pt3(x1 + x2, y1 + y2, z1 + z2)
    }
}
// lhs: Pt3 - rhs: Pt3 -> Vec3
impl Sub for Pt3 {
    type Output = Vec3;
    fn sub(self, other: Self) -> Vec3 {
        let Self(x1, y1, z1) = self;
        let Self(x2, y2, z2) = other;
        Vec3(x1 - x2, y1 - y2, z1 - z2)
    }
}

impl Pt3 {
    pub fn distance(self, other: Self) -> f64 {
        (other - self).mag()
    }
    pub fn lerp(self, other: Self, t: f64) -> Self {
        let Self(x1, y1, z1) = self;
        let Self(x2, y2, z2) = other;
        Self(x1 + (x2 - x1) * t, y1 + (y2 - y1) * t, z1 + (z2 - z1) * t)
    }
}

#[cfg(test)]
mod pt2_tests {
    use crate::{matrices::Mat3, points::Pt2, vectors::Vec2};

    #[test]
    fn test_default() {
        let p = Pt2::default();
        assert_eq!(p, Pt2(0.0, 0.0));
    }
    #[test]
    fn test_equal() {
        let p1 = Pt2(3.0, 4.0);
        let p2 = Pt2(3.0, 4.0);
        assert!(p1 == p2);
    }
    #[test]
    fn test_add() {
        let expect = Pt2(4.0, 6.0);
        let result = Pt2(3.0, 4.0) + Vec2(1.0, 2.0);
        assert_eq!(expect, result);
        let result = Vec2(1.0, 2.0) + Pt2(3.0, 4.0);
        assert_eq!(expect, result);
    }
    #[test]
    fn test_sub() {
        let expect = Vec2(5.0, 1.0);
        let result = Pt2(10.0, 7.0) - Pt2(5.0, 6.0);
        assert_eq!(expect, result);
    }
    #[test]
    fn test_distance() {
        let expect = 5.0;
        let result = Pt2(10.0, 0.0).distance(Pt2(5.0, 0.0));
        assert_eq!(expect, result);
    }
    #[test]
    fn test_lerp() {
        let expect = Pt2(5.0, 0.0);
        let result = Pt2(0.0, 0.0).lerp(Pt2(10.0, 0.0), 0.5);
        assert_eq!(expect, result);
    }
    #[test]
    fn test_transform_by() {
        let mat = Mat3::identity().translate(3.0, 4.0);
        let expect = Pt2(3.0, 4.0);
        let result = Pt2(0.0, 0.0).transform_by(mat);
        assert_eq!(expect, result);
    }
}

#[cfg(test)]
mod pt3_tests {
    use crate::{points::Pt3, vectors::Vec3};
    #[test]
    fn test_default() {
        let p = Pt3::default();
        assert_eq!(p, Pt3(0.0, 0.0, 0.0));
    }
    #[test]
    fn test_equal() {
        let p1 = Pt3(3.0, 4.0, 5.0);
        let p2 = Pt3(3.0, 4.0, 5.0);
        assert!(p1 == p2);
    }
    #[test]
    fn test_add() {
        let expect = Pt3(10.0, 9.0, 8.0);
        let result = Pt3(5.0, 4.5, 4.0) + Vec3(5.0, 4.5, 4.0);
        assert_eq!(expect, result);
        let result = Vec3(5.0, 4.5, 4.0) + Pt3(5.0, 4.5, 4.0);
        assert_eq!(expect, result);
    }
    #[test]
    fn test_sub() {
        let expect = Vec3(5.0, 1.0, 0.0);
        let result = Pt3(10.0, 7.0, 5.0) - Pt3(5.0, 6.0, 5.0);
        assert_eq!(expect, result);
    }
    #[test]
    fn test_distance() {
        let expect = 5.0;
        let result = Pt3(10.0, 0.0, 0.0).distance(Pt3(5.0, 0.0, 0.0));
        assert_eq!(expect, result);
    }
    #[test]
    fn test_lerp() {
        let expect = Pt3(5.0, 0.0, 0.0);
        let result = Pt3(0.0, 0.0, 0.0).lerp(Pt3(10.0, 0.0, 0.0), 0.5);
        assert_eq!(expect, result);
    }
}
