import math

# Vector2D(number, number) -> Vector2D
# Constructs a two-dimensional vector object.
# v = Vector2D(3, 4)
# v.x -> 3
# v.y -> 4
# v.point -> (3, 4)
# v.magnitude -> 5

class Vector2D:
    def __init__(self, x, y):
        self.x = x
        self.y = y
        self.point = (x, y)
        self.magnitude = math.hypot(x, y)
        
    # Vector2D.scale(number) -> Vector2D
    # Returns a scaled two-dimensional vector that is the product of a vector and a number.
    # Vector2D(3, 4).scale(2).point -> (6, 8)
    
    def scale(self, scalar):
        return Vector2D(self.x * scalar, self.y * scalar)
    
    # Vector2D.add(Vector2D) -> Vector2D
    # Returns a two-dimensional vector that is the sum of two vectors.
    # Vector2D(1, 2).add(Vector2D(3, 4)).point -> (4, 6)
    
    def add(self, vec):
        x1, y1 = self.point
        x2, y2 = vec.point
        return Vector2D(x1 + x2, y1 + y2)
    
    # Vector2D.dot(Vector2D) -> number
    # Returns a number that is the dot product of two, two-dimensional vectors.
    # Vector2D(1, 2).dot(Vector2D(3, 4)) -> 11
    
    def dot(self, vec):
        x1, y1 = self.point
        x2, y2 = vec.point
        return (x1 * x2) + (y1 * y2)
    
    # Vector2D.cross(Vector2D) -> number
    # Returns a number that is the cross product of two, two-dimensional vectors.
    # Vector2D(1, 2).cross(Vector2D(3, 4)) -> -2
    
    def cross(self, vec):
        x1, y1 = self.point
        x2, y2 = vec.point
        return (x1 * y2) - (y1 * x2)
    
    # Vector2D.round() -> Vector2D
    # Returns a two-dimensional vector with coordinate components rounded.
    # Vector2D(3.2, 4.7).round().point -> (3.0, 5.0)
    
    def round(self):
        return Vector2D(round(self.x), round(self.y))
    
    # Vector2D.eq(Vector2D) -> boolean
    # Checks whether the magnitudes of two vectors are equal.
    # Vector2D(3, 4).eq(Vector2D(3, 4)) -> True
    
    def eq(self, vec):
        return self.magnitude == vec.magnitude
    
    # Vector2D.gt(Vector2D) -> boolean
    # Checks whether the magnitude of vector one is greater than the magnitude of vector two.
    # Vector2D(3, 4).gt(Vector2D(1, 2)) -> True
    
    def gt(self, vec):
        return self.magnitude > vec.magnitude
    
    # Vector2D.lt(Vector2D) -> boolean
    # Checks whether the magnitude of vector one is less than the magnitude of vector two.
    # Vector2D(1, 2).lt(Vector2D(3, 4)) -> True
    
    def lt(self, vec):
        return self.magnitude < vec.magnitude

# IHAT, JHAT
# Mutually orthogonal two-dimensional unit vectors, forming the standard basis.

IHAT = Vector2D(1, 0)
JHAT = Vector2D(0, 1)

