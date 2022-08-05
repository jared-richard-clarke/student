import math

"""
Provides affine transformation matrices, methods, and functions.
"""


class Mat3:
    """
    A 2d transformation implemented as a column-major, 3 × 3 matrix.
    The third row is implicit.
    """

    def __init__(self, xx, yx, xy, yy, x0, y0):
        self.xx = xx
        self.yx = yx
        self.xy = xy
        self.yy = yy
        self.x0 = x0
        self.y0 = y0

    def __str__(self):
        return f"Mat3({self.xx}, {self.yx}, {self.xy}, {self.yy}, {self.x0}, {self.y0})"

    def __mul__(self, other):
        """Combines two transformations through matrix multiplication."""
        return Mat3(self.xx * other.xx + self.yx * other.xy,
                    self.xx * other.yx + self.yx * other.yy,
                    self.xy * other.xx + self.yy * other.xy,
                    self.xy * other.yx + self.yy * other.yy,
                    self.x0 * other.xx + self.y0 * other.xy + other.x0,
                    self.x0 * other.yx + self.y0 * other.yy + other.y0)

    def translate(self, x, y):
        """
        Translates matrix by scalars "x" and "y". 
        Transformation can be chained.
        """
        return translate(x, y) * self

    def scale(self, x, y):
        """
        Scales matrix by scalars "x" and "y". 
        Transformation can be chained.
        """
        return scale(x, y) * self

    def rotate(self, angle):
        """
        Rotates matrix by "angle", measured in radians. 
        Transformation can be chained.
        """
        return rotate(angle) * self

    def shear(self, x, y):
        """
        Shears matrix by scalars "x" and "y". Transformation can be chained.
        """
        return shear(x, y) * self


def identity():
    """Creates an identity matrix."""
    return Mat3(1, 0,
                0, 1,
                0, 0)


def translate(x, y):
    """Creates a translation matrix."""
    return Mat3(1, 0,
                0, 1,
                x, y)


def scale(x, y):
    """Creates a scaling matrix."""
    return Mat3(x, 0,
                0, y,
                0, 0)


def rotate(angle):
    """Creates a rotation matrix."""
    c = math.cos(angle)
    s = math.sin(angle)
    return Mat3(c, s,
                -s, c,
                0, 0)


def shear(x, y):
    """Creates a shearing matrix."""
    return Mat3(1, y,
                x, 1,
                0, 0)
