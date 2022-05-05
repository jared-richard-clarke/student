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

# IHAT, JHAT
# Mutually orthogonal two-dimensional unit vectors, forming the standard basis.

IHAT = Vector2D(1, 0)
JHAT = Vector2D(0, 1)
    
# scale(Vector2D, number) -> Vector2D
# Returns a scaled two-dimensional vector that is the product of a vector and a number.
# v = scale(Vector2D(3, 4), 2) ->  v.point -> (6, 8)

def scale(v, scalar):
    x, y = v.point
    return Vector2D((x * scalar), (y * scalar))

# add(Vector2D, Vector2D) -> Vector2D
# Returns a two-dimensional vector that is the sum of two vectors.
# v = add(Vector2D(1, 2), Vector2D(3, 4)) -> v.point -> (4, 6)

def add(v1, v2):
    x1, y1 = v1.point
    x2, y2 = v2.point
    return Vector2D(x1 + x2, y1 + y2)

# dot(Vector2D, Vector2D) -> number
# Returns a number that is the dot product of two, two-dimensional vectors.
# dot(Vector2D(1, 2), Vector2D(3, 4)) -> 11

def dot(v1, v2):
    x1, y1 = v1.point
    x2, y2 = v2.point
    return (x1 * x2) + (y1 * y2)

# cross(Vector2D, Vector2D) -> number
# Returns a number that is the cross product of two, two-dimensional vectors.
# cross(Vector2D(1, 2), Vector2D(3, 4)) -> -2

def cross(v1, v2):
    x1, y1 = v1.point
    x2, y2 = v2.point
    return (x1 * y2) - (y1 * x2)

# compare(function) -> function -> boolean
# Generates functions for sequentially comparing the magnitudes of a series of two-dimensional vectors.
# eq = compare(lambda x, y: x == y) -> eq(Vector2D(3, 4), Vector2D(3, 4)) -> True

def compare(operation):
    def operator(v1, v2):
        m1 = v1.magnitude
        m2 = v2.magnitude
        return operation(m1, m2)
    return operator

gt = compare(lambda x, y: x > y)
lt = compare(lambda x, y: x < y)
eq = compare(lambda x, y: x == y)

# approximate(function) -> function(Vector2D) -> Vector2D
# Generates approximation functions for simplifying vector components.
# vec_round = approximate(round) -> vec_round(Vector2D(1.3, 1.7)) -> Vector2D.point -> (1.0, 2.0)

def approximate(operation): 
    def approximater(vec):
        x, y = vec.point
        return Vector2D(operation(x), operation(y))
    return approximater

vec_round = approximate(round)
vec_ceiling = approximate(math.ceil)
vec_floor = approximate(math.floor)
