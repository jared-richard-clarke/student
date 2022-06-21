import math

from vec2.oop import Vec2

"""
A functional implementation of a two-dimensional vector,
associated functions, and constants.
"""

# vec2(number, number) -> tuple(number, number)
# Constructs a two-dimensional vector implemented as a tuple.
# vec2(3, 4) -> (3, 4)

def vec2(x, y):
    return (x, y)

# add(*tuple(number, number)) -> tuple(number, number)
# Returns a two-dimensional vector that is the sum of a series of two-dimensional vectors.
# add(vec2(1, 2), vec2(3, 4), vec2(0, 2)) -> (4, 8)

def add(*vecs):
    sum = (0, 0)
    for vec in vecs:
        x1, y1 = sum
        x2, y2 = vec
        sum = (x1 + x2, y1 + y2)
    return sum

# negate(tuple(number, number)) -> tuple(number, number)
# Inverts the signs of the vector components. Flips the vector 180 degrees.
# negate(vec2(3, 4)) -> vec2(-3, -4)

def negate(vec):
    x, y = vec
    return vec2(-x, -y)

# scale(tuple(number, number), number) -> tuple(number, number)
# Returns a scaled two-dimensional vector that is the product of a vector and a number.
# scale(vec2(3, 4), 2) -> (6, 8)

def scale(vec, scalar):
    x, y = vec
    return vec2(x * scalar, y * scalar)

# dot(tuple(number, number), tuple(number, number)) -> number
# Returns the dot product of two, two-dimensional vectors
# dot(vec2(1, 2), vec2(3, 4)) -> 11

def dot(v1, v2):
    x1, y1 = v1
    x2, y2 = v2
    return (x1 * x2) + (y1 * y2)

# normalize(tuple(number, number), tuple(number, number)) -> tuple(number, number)
# Returns the unit vector of a two-dimensional vector.
# normalize(3, 4) -> (0.6, 0.8)

def normalize(vec):
    mag = magnitude(vec)
    x, y = vec
    return vec2(x / mag, y / mag)

# magnitude(tuple(number, number)) -> number
# Computes the magnitude of a two-dimensional vector.
# magnitude(vec2(3, 4)) -> 5.0

def magnitude(vec):
    x, y = vec
    return math.hypot(x, y)

# distance(tuple(number, number), tuple(number, number)) -> number
# Returns the distance between two vector points.
# distance(vec2(8, 0), vec2(1, 0)) -> 7.0

def distance(v1, v2):
    x1, y1 = v1
    x2, y2 = v2
    return math.hypot(x2 - x1, y2 - y1)

# lerp(tuple(number, number), tuple(number, number), number) -> tuple(number, number)
# Interpolates a vector point along the line between two vector points.
# lerp(vec2(0, 10), vec2(8, -4), -1) -> vec2(-8, 24)

def lerp(v1, v2, t):
    x1, y1 = v1
    x2, y2 = v2
    x = x1 + (x2 - x1) * t
    y = y1 + (y2 - y1) * t
    return vec2(x, y)

# equal(tuple(number, number), tuple(number, number)) -> boolean
# Compares the components of two vectors. Checks for equality.
# Comparisons are applied left to right.
# equal(vec2(3, 4), vec2(3, 4)) -> True

def equal(v1, v2):
    x1, y1 = v1
    x2, y2 = v2
    return x1 == x2 and y1 == y2

# approximate(function) -> function(tuple(number, number)) -> tuple(number, number)
# Constructs approximation functions for rounding vector components to integers.
# rnd = approximate(round) -> rnd(vec2(1.3, 1.7)) -> (1.0, 2.0)

def approximate(operation): 
    def approximater(vec):
        x, y = vec
        return vec2(operation(x), operation(y))
    return approximater

rnd = approximate(round)
ceil = approximate(math.ceil)
floor = approximate(math.floor)
