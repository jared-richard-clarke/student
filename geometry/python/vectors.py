import math
import approximate

"""
Provides cartesian vectors and methods.
"""


class Vec2:
    """
    Creates a cartesian representation of a vector in two dimensions.
    """

    def __init__(self, x, y):
        self.x = x
        self.y = y
        self.point = (x, y)

    def __add__(self, other):
        """Returns the sum of two vectors."""
        x1, y1 = self.point
        x2, y2 = other.point
        return Vec2(x1 + x2, y1 + y2)

    def __sub__(self, other):
        """Returns the difference of two vectors."""
        x1, y1 = self.point
        x2, y2 = other.point
        return Vec2(x1 - x2, y1 - y2)

    def __neg__(self):
        """
        Inverts the signs of the vector components. 
        Flips the vector 180 degrees
        """
        return Vec2(-self.x, -self.y)

    def mag(self):
        """Returns the magnitude of a vector."""
        x, y = self.point
        return math.hypot(x, y)

    def scale(self, scalar):
        """Returns a scaled two-dimensional vector."""
        return Vec2(self.x * scalar, self.y * scalar)

    def dot(self, other):
        """Returns the dot product of two vectors."""
        x1, y1 = self.point
        x2, y2 = other.point
        return (x1 * x2) + (y1 * y2)

    def normalize(self):
        """Returns the unit vector of a vector."""
        x, y = self.point
        mag = self.mag()
        return Vec2(x / mag, y / mag)

    def distance(self, other):
        """Returns the distance between the tips of two vectors."""
        x1, y1 = self.point
        x2, y2 = other.point
        return math.hypot(x2 - x1, y2 - y1)

    def lerp(self, other, t):
        """Interpolates a vector point between the tips of two vectors."""
        x = self.x + (other.x - self.x) * t
        y = self.y + (other.y - self.y) * t
        return Vec2(x, y)

    def round(self):
        """Returns a vector with its coordinate components rounded."""
        return Vec2(round(self.x), round(self.y))

    def __eq__(self, other):
        """Checks for equality by comparing vector components."""
        x1, y1 = self.point
        x2, y2 = other.point
        return x1 == x2 and y1 == y2

    def equals(self, other):
        """
        As opposed to operator "==", method "equals" checks whether 
        floating-point vector components are approximately equal.
        """
        x1, y1 = self.point
        x2, y2 = other.point
        eq = approximate.equals
        return eq(x1, x2) and eq(y1, y2)

    def __str__(self):
        """Returns a textual representation of a Vec2 object."""
        return f"vec2({self.x},{self.y})"
