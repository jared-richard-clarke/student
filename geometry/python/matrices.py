import math
import approximate

"""
Provides affine transformation matrices, methods, and functions.
"""


class Mat3:
    """
    A 2d transformation implemented as a column-major, 3 × 3 matrix.
    The third row is implicit.
    """

    def __init__(self, a, b, c, d, e, f):
        self.a = a
        self.b = b
        self.c = c
        self.d = d
        self.e = e
        self.f = f

    def __eq__(self, other):
        """Checks equality by comparing matrix components."""
        return (self.a == other.a and
                self.b == other.b and
                self.c == other.c and
                self.d == other.d and
                self.e == other.e and
                self.f == other.f)

    def approx_eq(self, other):
        """
        As opposed to operator "==", method "approx_eq" checks whether 
        floating-point matrix components are approximately equal.
        """
        eq = approximate.equals
        return (eq(self.a, other.a) and
                eq(self.b, other.b) and
                eq(self.c, other.c) and
                eq(self.d, other.d) and
                eq(self.e, other.e) and
                eq(self.f, other.f))

    def __mul__(self, other):
        """Combines two transformations through matrix multiplication."""
        return Mat3(self.a * other.a + self.b * other.c,
                    self.a * other.b + self.b * other.d,
                    self.c * other.a + self.d * other.c,
                    self.c * other.b + self.d * other.d,
                    self.e * other.a + self.f * other.c + other.e,
                    self.e * other.b + self.f * other.d + other.f)

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

    def __str__(self):
        return f"Mat3({self.a}, {self.b}, {self.c}, {self.d}, {self.e}, {self.f})"


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
