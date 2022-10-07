use crate::points::Point2D;
use crate::vectors::Vector2D;
use std::ops::Mul;

#[derive(Clone, Copy, PartialEq, Debug, Default)]
pub struct Mat3 {
    pub a: f64,
    pub b: f64,
    pub c: f64,
    pub d: f64,
    pub e: f64,
    pub f: f64,
}

pub fn mat3(a: f64, b: f64, c: f64, d: f64, e: f64, f: f64) -> Mat3 {
    Mat3 { a, b, c, d, e, f }
}

impl Mul for Mat3 {
    type Output = Self;
    fn mul(self, other: Self) -> Self {
        Mat3 {
            a: self.a * other.a + self.b * other.c,
            b: self.a * other.b + self.b * other.d,
            c: self.c * other.a + self.d * other.c,
            d: self.c * other.b + self.d * other.d,
            e: self.e * other.a + self.f * other.c + other.e,
            f: self.e * other.b + self.f * other.d + other.f,
        }
    }
}

fn translate(x: f64, y: f64) -> Mat3 {
    Mat3 {
        a: 1.0,
        b: 0.0,
        c: 0.0,
        d: 1.0,
        e: x,
        f: y,
    }
}

fn scale(x: f64, y: f64) -> Mat3 {
    Mat3 {
        a: x,
        b: 0.0,
        c: 0.0,
        d: y,
        e: 0.0,
        f: 0.0,
    }
}

fn rotate(angle: f64) -> Mat3 {
    let c = angle.cos();
    let s = angle.sin();
    Mat3 {
        a: c,
        b: s,
        c: -s,
        d: c,
        e: 0.0,
        f: 0.0,
    }
}

fn shear(x: f64, y: f64) -> Mat3 {
    Mat3 {
        a: 1.0,
        b: y,
        c: x,
        d: 1.0,
        e: 0.0,
        f: 0.0,
    }
}

impl Mat3 {
    pub fn identity() -> Self {
        Self {
            a: 1.0,
            b: 0.0,
            c: 0.0,
            d: 1.0,
            e: 0.0,
            f: 0.0,
        }
    }
    pub fn translate(self, x: f64, y: f64) -> Self {
        translate(x, y) * self
    }
    pub fn scale(self, x: f64, y: f64) -> Self {
        scale(x, y) * self
    }
    pub fn rotate(self, angle: f64) -> Self {
        rotate(angle) * self
    }
    pub fn shear(self, x: f64, y: f64) -> Self {
        shear(x, y) * self
    }
    pub fn transform_vector(self, v: Vector2D) -> Vector2D {
        Vector2D {
            x: self.a * v.x + self.c * v.y,
            y: self.b * v.x + self.d * v.y,
        }
    }
    pub fn transform_point(self, p: Point2D) -> Point2D {
        Point2D {
            x: self.a * p.x + self.c * p.y + self.e,
            y: self.b * p.x + self.d * p.y + self.f,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        matrices::{mat3, Mat3},
        points::pt2,
        vectors::vec2,
    };

    #[test]
    fn test_default() {
        let expect = mat3(0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
        let result = Mat3::default();
        assert_eq!(result, expect);
    }

    #[test]
    fn test_identity() {
        let expect = mat3(1.0, 0.0, 0.0, 1.0, 0.0, 0.0);
        let result = Mat3::identity();
        assert_eq!(result, expect);
    }

    #[test]
    fn test_translate() {
        let expect = mat3(1.0, 0.0, 0.0, 1.0, 3.0, 4.0);
        let result = Mat3::identity().translate(3.0, 4.0);
        assert_eq!(result, expect);
    }

    #[test]
    fn test_scale() {
        let expect = mat3(3.0, 0.0, 0.0, 4.0, 0.0, 0.0);
        let result = Mat3::identity().scale(3.0, 4.0);
        assert_eq!(result, expect);
    }

    #[test]
    fn test_rotate() {
        let expect = Mat3::identity();
        let result = Mat3::identity()
            .rotate((90.0_f64).to_radians())
            .rotate((-90.0_f64).to_radians());
        assert_eq!(result, expect);
    }

    #[test]
    fn test_shear() {
        let expect = mat3(1.0, 4.0, 3.0, 1.0, 0.0, 0.0);
        let result = Mat3::identity().shear(3.0, 4.0);
        assert_eq!(result, expect);
    }

    #[test]
    fn test_transform() {
        let expect = mat3(2.0, 4.0, 2.0, 2.0, 3.0, 4.0);
        let result = Mat3::identity()
            .translate(3.0, 4.0)
            .scale(2.0, 2.0)
            .shear(1.0, 2.0);
        assert_eq!(result, expect);
    }

    #[test]
    fn test_transform_vector() {
        let expect = vec2(6.0, 8.0);
        let result = Mat3::identity()
            .scale(2.0, 2.0)
            .transform_vector(vec2(3.0, 4.0));
        assert_eq!(expect, result);
    }

    #[test]
    fn test_transform_point() {
        let expect = pt2(4.0, 6.0);
        let result = Mat3::identity()
            .translate(3.0, 4.0)
            .transform_point(pt2(1.0, 2.0));
        assert_eq!(expect, result);
    }
}
