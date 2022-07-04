use std::ops::Mul;

#[rustfmt::skip]
#[derive(Clone, Copy, PartialEq, Debug, Default)]
pub struct Mat3 {
    pub xx: f64, pub yx: f64,
    pub xy: f64, pub yy: f64,
    pub x0: f64, pub y0: f64,
}

#[rustfmt::skip]
pub fn mat3(xx: f64, yx: f64, xy: f64, yy: f64, x0: f64, y0: f64) -> Mat3 {
    Mat3 {
        xx, yx,
        xy, yy,
        x0, y0,
    }
}

#[rustfmt::skip]
fn translate(x: f64, y: f64) -> Mat3 {
    Mat3 {
        xx: 1.0, yx: 0.0,
        xy: 0.0, yy: 1.0,
        x0: x,   y0: y,
    }
}

#[rustfmt::skip]
fn scale(x: f64, y: f64) -> Mat3 {
    Mat3 {
        xx: x,   yx: 0.0,
        xy: 0.0, yy: y,
        x0: 0.0, y0: 0.0,
    }
}

#[rustfmt::skip]
fn rotate(angle: f64) -> Mat3 {
    let c = angle.cos();
    let s = angle.sin();
    Mat3 {
        xx: c,   yx: s,
        xy: -s,  yy: c,
        x0: 0.0, y0: 0.0,
    }
}

#[rustfmt::skip]
fn shear(x: f64, y: f64) -> Mat3 {
    Mat3 {
        xx: 1.0, yx: y,
        xy: x,   yy: 1.0,
        x0: 0.0, y0: 0.0,
    }
}

impl Mat3 {
    #[rustfmt::skip]
    pub fn identity() -> Self {
        Self {
            xx: 1.0, yx: 0.0,
            xy: 0.0, yy: 1.0,
            x0: 0.0, y0: 0.0,
        }
    }
    pub fn translate(self, x: f64, y: f64) -> Self {
        self * translate(x, y)
    }
    pub fn scale(self, x: f64, y: f64) -> Self {
        self * scale(x, y)
    }
    pub fn rotate(self, angle: f64) -> Self {
        self * rotate(angle)
    }
    pub fn shear(self, x: f64, y: f64) -> Self {
        self * shear(x, y)
    }
}

impl Mul for Mat3 {
    type Output = Self;
    fn mul(self, other: Self) -> Self {
        Mat3 {
            xx: self.xx * other.xx + self.yx * other.xy,
            yx: self.xx * other.yx + self.yx * other.yy,
            xy: self.xy * other.xx + self.yy * other.xy,
            yy: self.xy * other.yx + self.yy * other.yy,
            x0: self.x0 * other.xx + self.y0 * other.xy + other.x0,
            y0: self.x0 * other.yx + self.y0 * other.yy + other.y0,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::matrices::{mat3, Mat3};

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
        let expect = mat3(
            0.2836621854632263,
            -0.9589242746631385,
            0.9589242746631385,
            0.2836621854632263,
            0.0,
            0.0,
        );
        let result = Mat3::identity().rotate(5.0);
        assert_eq!(result, expect);
    }

    #[test]
    fn test_shear() {
        let expect = mat3(1.0, 4.0, 3.0, 1.0, 0.0, 0.0);
        let result = Mat3::identity().shear(3.0, 4.0);
        assert_eq!(result, expect);
    }
}
