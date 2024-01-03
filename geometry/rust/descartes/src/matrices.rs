use std::ops::Mul;

/// A column-major, 3 x 3 affine transformation matrix. The third row is implicit.
/// Constant fields are implied.
///
/// ( a b c d ) <- linear transformations
///
/// ( e f ) <----- translations
#[derive(Clone, Copy, PartialEq, Debug, Default)]
pub struct Mat3(pub f64, pub f64, pub f64, pub f64, pub f64, pub f64);

// Combines matrix transformations through multiplication.
// Overloads the `*` operator for `Mat3`.
impl Mul for Mat3 {
    type Output = Self;
    fn mul(self, other: Self) -> Self {
        let Self(a1, b1, c1, d1, e1, f1) = self;
        let Self(a2, b2, c2, d2, e2, f2) = other;
        Self(
            a1 * a2 + b1 * c2,
            a1 * b2 + b1 * d2,
            c1 * a2 + d1 * c2,
            c1 * b2 + d1 * d2,
            e1 * a2 + f1 * c2 + e2,
            e1 * b2 + f1 * d2 + f2,
        )
    }
}

fn translate(x: f64, y: f64) -> Mat3 {
    Mat3(1.0, 0.0, 0.0, 1.0, x, y)
}

fn scale(x: f64, y: f64) -> Mat3 {
    Mat3(x, 0.0, 0.0, y, 0.0, 0.0)
}

fn rotate(angle: f64) -> Mat3 {
    let c = angle.cos();
    let s = angle.sin();
    Mat3(c, s, -s, c, 0.0, 0.0)
}

fn shear(x: f64, y: f64) -> Mat3 {
    Mat3(1.0, y, x, 1.0, 0.0, 0.0)
}

impl Mat3 {
    /// Creates a 3 x 3 identity matrix: `Mat3(1, 0, 0, 1, 0, 0)`
    pub fn identity() -> Self {
        Self(1.0, 0.0, 0.0, 1.0, 0.0, 0.0)
    }
    /// Translates matrix by scalars `x` and `y`. Transformations can be chained.
    pub fn translate(self, x: f64, y: f64) -> Self {
        translate(x, y) * self
    }
    /// Scales matrix by scalars `x` and `y`. Transformations can be chained.
    pub fn scale(self, x: f64, y: f64) -> Self {
        scale(x, y) * self
    }
    /// Rotates matrix by `angle`, which is measured in radians. Transformations can be chained.
    pub fn rotate(self, angle: f64) -> Self {
        rotate(angle) * self
    }
    /// Shears matrix by scalars `x` and `y`. Transformations can be chained.
    pub fn shear(self, x: f64, y: f64) -> Self {
        shear(x, y) * self
    }
}

#[cfg(test)]
mod tests {
    use crate::matrices::Mat3;

    #[test]
    fn test_default() {
        let expect = Mat3(0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
        let result = Mat3::default();
        assert_eq!(result, expect);
    }

    #[test]
    fn test_identity() {
        let expect = Mat3(1.0, 0.0, 0.0, 1.0, 0.0, 0.0);
        let result = Mat3::identity();
        assert_eq!(result, expect);
    }

    #[test]
    fn test_translate() {
        let expect = Mat3(1.0, 0.0, 0.0, 1.0, 3.0, 4.0);
        let result = Mat3::identity().translate(3.0, 4.0);
        assert_eq!(result, expect);
    }

    #[test]
    fn test_scale() {
        let expect = Mat3(3.0, 0.0, 0.0, 4.0, 0.0, 0.0);
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
        let expect = Mat3(1.0, 4.0, 3.0, 1.0, 0.0, 0.0);
        let result = Mat3::identity().shear(3.0, 4.0);
        assert_eq!(result, expect);
    }

    #[test]
    fn test_compose() {
        let expect = Mat3(2.0, 4.0, 2.0, 2.0, 3.0, 4.0);
        let result = Mat3::identity()
            .translate(3.0, 4.0)
            .scale(2.0, 2.0)
            .shear(1.0, 2.0);
        assert_eq!(result, expect);
    }
}

