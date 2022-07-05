import math

"""
An Object-Oriented implementation of a two-dimensional vector,
and associated methods.
"""

# Vec2(number, number) -> Vec2
# Constructs a two-dimensional vector object.
# v = Vec2(3, 4)
# v.x -> 3
# v.y -> 4
# v.point -> (3, 4)


class Vec2:
    def __init__(self, x, y):
        self.x = x
        self.y = y
        self.point = (x, y)

    # Vec2 + Vec2 -> Vec2
    # Returns a two-dimensional vector that is the sum of two vectors.
    # Implements __add__.
    # (Vec2(1, 2) + Vec2(3, 4)).point -> (4, 6)

    def __add__(self, other):
        x1, y1 = self.point
        x2, y2 = other.point
        return Vec2(x1 + x2, y1 + y2)

    # Vec2 - Vec2 -> Vec2
    # Returns a two-dimensional vector that is the difference of two vectors.
    # Implements __sub__.
    # (Vec2(3, 4) - Vec2(1, 2)).point -> (2, 2)

    def __sub__(self, other):
        x1, y1 = self.point
        x2, y2 = other.point
        return Vec2(x1 - x2, y1 - y2)

    # -Vec2 -> -Vec2
    # Inverts the signs of the vector components. Flips the vector 180 degrees.
    # -Vec2(3, 4) -> Vec2(-3, -4)

    def __neg__(self):
        return Vec2(-self.x, -self.y)

    # Vec2.mag() -> number
    # Returns the mag/magnitude of a vector.
    # Vec2(3, 4).mag() -> 5

    def mag(self):
        x, y = self.point
        return math.hypot(x, y)

    # Vec2.scale(number) -> Vec2
    # Returns a scaled two-dimensional vector that is the product of a vector and a number.
    # Vec2(3, 4).scale(2).point -> (6, 8)

    def scale(self, scalar):
        return Vec2(self.x * scalar, self.y * scalar)

    # Vec2.dot(Vec2) -> number
    # Returns a number that is the dot product of two, two-dimensional vectors.
    # Vec2(1, 2).dot(Vec2(3, 4)) -> 11

    def dot(self, other):
        x1, y1 = self.point
        x2, y2 = other.point
        return (x1 * x2) + (y1 * y2)

    # Vec2.normalize() -> Vec2
    # Returns the unit vector of a two-dimensional vector.
    # Vec2(3, 4).normalize() -> Vec2(0.6, 0.8)

    def normalize(self):
        x, y = self.point
        mag = self.mag()
        return Vec2(x / mag, y / mag)

    # Vec2.distance(Vec2) -> number
    # Returns the distance between two vector points.
    # Vec2(8, 0).distance(Vec2(1, 0)) -> 7.0

    def distance(self, other):
        x1, y1 = self.point
        x2, y2 = other.point
        return math.hypot(x2 - x1, y2 - y1)

    # Vec2.lerp(Vec2) -> Vec2
    # Interpolates a vector point between two vector points.
    # Vec2(0, 10).lerp(Vec2(8, -4), -1) -> Vec2(-8, 24)

    def lerp(self, other, t):
        x = self.x + (other.x - self.x) * t
        y = self.y + (other.y - self.y) * t
        return Vec2(x, y)

    # Vec2.round() -> Vec2
    # Returns a two-dimensional vector with coordinate components rounded.
    # Vec2(3.2, 4.7).round().point -> (3.0, 5.0)

    def round(self):
        return Vec2(round(self.x), round(self.y))

    # Vec2 == Vec2 -> boolean
    # Compares the components of vectors. Checks for equality.
    # Implements __eq__.
    # Vec2(3, 4) == Vec2(3, 4) -> True

    def __eq__(self, other):
        x1, y1 = self.point
        x2, y2 = other.point
        return x1 == x2 and y1 == y2

    # Vec2(3, 4).__str__() -> string
    # Returns a textual representation of a Vec2 object.
    # Implements __str__.

    def __str__(self):
        return f"vec({self.x},{self.y})"
