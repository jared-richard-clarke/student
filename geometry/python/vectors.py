import math
import utils

"""
Provides cartesian vectors and methods.
"""


class Vector:
    """
    Base class for a cartesian representation of a vector.
    """

    def __init__(self, *xs):
        self.point = xs

    def __eq__(self, other):
        """Checks equality by comparing vector components."""
        return self.point == other.point

    def approx_eq(self, other):
        """
        As opposed to operator "==", method "approx_eq" checks whether 
        floating-point vector components are approximately equal.
        """
        eq = utils.equals
        for x1, x2 in zip(self.point, other.point, strict=True):
            if not eq(x1, x2):
                return False
        return True

    def __abs__(self):
        """Returns a vector with the absolute values of the original vector components."""
        result = [abs(x) for x in self.point]
        return type(self)(*result)

    def __add__(self, other):
        """Returns the sum of two vectors."""
        result = [x + y for x, y in zip(self.point, other.point, strict=True)]
        return type(self)(*result)

    def __sub__(self, other):
        """Returns the difference between two vectors."""
        result = [x - y for x, y in zip(self.point, other.point, strict=True)]
        return type(self)(*result)

    def __neg__(self):
        """Inverts the signs of the vector components."""
        result = [-x for x in self.point]
        return type(self)(*result)

    def invert(self):
        """Inverts the vector components."""
        result = [1 / x for x in self.point]
        return type(self)(*result)

    def mag(self):
        """Returns the magnitude of a vector."""
        return math.hypot(*self.point)

    def scale(self, scalar):
        """Returns a scaled two-dimensional vector."""
        result = [x * scalar for x in self.point]
        return type(self)(*result)

    def dot(self, other):
        """Returns the dot product of two vectors."""
        result = sum(
            [v1 * v2 for v1, v2 in zip(self.point, other.point, strict=True)])
        return result

    def distance(self, other):
        """Returns the distance between the tips of two vectors."""
        result = [x2 - x1 for x1,
                  x2 in zip(self.point, other.point, strict=True)]
        return math.hypot(*result)

    def lerp(self, other, t):
        """Interpolate the components of two vectors."""
        result = [x1 + (x2 - x1) * t for x1,
                  x2 in zip(self.point, other.point, strict=True)]
        return type(self)(*result)

    def normalize(self):
        """Returns the unit vector of a vector."""
        mag = self.mag()
        result = [x / mag for x in self.point]
        return type(self)(*result)

    def round(self):
        """Returns a vector with its coordinate components rounded."""
        result = [round(x) for x in self.point]
        return type(self)(*result)


class Vec2(Vector):
    """
    A cartesian representation of a two-dimensional vector.
    """

    def __init__(self, x, y):
        super().__init__(x, y)
        self.x = x
        self.y = y

    def __str__(self):
        return f"Vec2{self.point}"


class Vec3(Vector):
    """
    A cartesian representation of a three-dimensional vector.
    """

    def __init__(self, x, y, z):
        super().__init__(x, y, z)
        self.x = x
        self.y = y
        self.z = z

    def __str__(self):
        return f"Vec3{self.point}"
