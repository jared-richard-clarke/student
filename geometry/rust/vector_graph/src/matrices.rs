use std::ops::Mul;

#[derive(Clone, Copy, PartialEq, Debug, Default)]
pub struct Mat3 {
    pub xx: f64,
    pub yx: f64,
    pub xy: f64,
    pub yy: f64,
    pub x0: f64,
    pub y0: f64,
}

pub fn mat3(xx: f64, yx: f64, xy: f64, yy: f64, x0: f64, y0: f64) -> Mat3 {
    Mat3 {
        xx,
        yx,
        xy,
        yy,
        x0,
        y0,
    }
}

fn translate(x: f64, y: f64) -> Mat3 {
    Mat3 {
        xx: 1.0,
        yx: 0.0,
        xy: 0.0,
        yy: 1.0,
        x0: x,
        y0: y,
    }
}

fn scale(x: f64, y: f64) -> Mat3 {
    Mat3 {
        xx: x,
        yx: 0.0,
        xy: 0.0,
        yy: y,
        x0: 0.0,
        y0: 0.0,
    }
}

fn rotate(angle: f64) -> Mat3 {
    let c = angle.cos();
    let s = angle.sin();
    Mat3 {
        xx: c,
        yx: s,
        xy: -s,
        yy: c,
        x0: 0.0,
        y0: 0.0,
    }
}

fn shear(x: f64, y: f64) -> Mat3 {
    Mat3 {
        xx: 1.0,
        yx: y,
        xy: x,
        yy: 1.0,
        x0: 0.0,
        y0: 0.0,
    }
}

impl Mat3 {
    pub fn identity() -> Self {
        Self {
            xx: 1.0,
            yx: 0.0,
            xy: 0.0,
            yy: 1.0,
            x0: 0.0,
            y0: 0.0,
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
