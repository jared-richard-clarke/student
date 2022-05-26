import math

"""
A functional implementation of a two-dimensional vector,
associated functions, and constants.
"""

# vec2(number, number) -> tuple(number, number)
# Constructs a two-dimensional vector implemented as a tuple.
# vec2(3, 4) -> (3, 4)

def vec2(x, y):
    return (x, y)

# IHAT, JHAT
# Mutually orthogonal two-dimensional unit vectors, forming the standard basis.

IHAT = vec2(1, 0)
JHAT = vec2(0, 1)

# flip(tuple(number, number)) -> tuple(number, number)
# Inverts the signs of the vector components. Flips the vector 180 degrees.
# flip(vec2(3, 4)) -> vec2(-3, -4)

def flip(vec):
    x, y = vec
    return vec2(-1 * x, -1 * y)

# magnitude(tuple(number, number)) -> number
# Computes the magnitude of a two-dimensional vector.
# magnitude(vec2(3, 4)) -> 5.0

def magnitude(vec):
    x, y = vec
    return math.hypot(x, y)

# distance(tuple(number, number), tuple(number, number)) -> number
# Returns the distance between two, two-dimensional vectors.
# distance(vec2(8, 0), vec2(1, 0)) -> 7.0

def distance(v1, v2):
    x1, y1 = v1
    x2, y2 = v2
    return math.hypot(x2 - x1, y2 - y1)

# scale(tuple(number, number), number) -> tuple(number, number)
# Returns a scaled two-dimensional vector that is the product of a vector and a number.
# scale(vec2(3, 4), 2) -> (6, 8)

def scale(vec, scalar):
    x, y = vec
    return vec2(x * scalar, y * scalar)

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

# compare(function) -> function(tuple(number, number), tuple(number, number)) -> boolean
# Generates functions for comparing the components of two, two-dimensional vectors.
# Comparisons are applied left to right.
# eq = compare(lambda x, y: x == y) -> eq(vec2(3, 4), vec2(3, 4)) -> True

def compare(operation):
    def comparison(v1, v2):
        x1, y1 = v1
        x2, y2 = v2
        return operation(x1, x2) and operation(y1, y2)
    return comparison

eq = compare(lambda x, y: x == y)
gt = compare(lambda x, y: x > y)
ge = compare(lambda x, y: x >= y)
lt = compare(lambda x, y: x < y)
le = compare(lambda x, y: x <= y)

# approximate(function) -> function(tuple(number, number)) -> tuple(number, number)
# Constructs approximation functions for rounding vector components to integers.
# rnd = approximate(round) -> rnd(vec2(1.3, 1.7)) -> (1.0, 2.0)

def approximate(operation): 
    def approximater(vec):
        x, y = vec
        return vec2(operation(x), operation(y))
    return approximater

rnd = approximate(round)
ceiling = approximate(math.ceil)
floor = approximate(math.floor)
