import math

"""
A functional implementation of a two-dimensional vector
and associated functions and constants.
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

# magnitude(tuple(number, number)) -> number
# Computes the magnitude of a two-dimensional vector.
# magnitude(vec2(3, 4)) -> 5.0

def magnitude(vec):
    x, y = vec
    return math.hypot(x, y)

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
# Computes the dot product of two, two-dimensional vectors
# dot(vec2(1, 2), vec2(3, 4)) -> 11

def dot(v1, v2):
    x1, y1 = v1
    x2, y2 = v2
    return (x1 * x2) + (y1 * y2)

# cross(tuple(number, number), tuple(number, number)) -> number
# Computes the cross product of two, two-dimensional vectors.
# cross(vec2(1, 2), vec2(3, 4)) -> -2

def cross(v1, v2):
    x1, y1 = v1
    x2, y2 = v2
    return (x1 * y2) - (y1 * x2)

# compare(function) -> function(tuple(number, number), tuple(number, number)) -> boolean
# Constructs comparison functions that operates on the magnitudes of two-dimensional vectors.
# eq = compare(lambda x, y: x == y) -> eq(vec2(3, 4), vec2(3, 4)) -> True

def compare(operation):
    def comparison(v1, v2):
        m1 = magnitude(v1.x, v1.y)
        m2 = magnitude(v2.x, v2.y)
        return operation(m1, m2)
    return comparison

eq = compare(lambda x, y: x == y)
gt = compare(lambda x, y: x > y)
lt = compare(lambda x, y: x < y)

# approximate(function) -> function(vec2) -> vec2
# Constructs approximation functions for simplifying vector components.
# rnd = approximate(round) -> rnd(vec2(1.3, 1.7)) -> (1.0, 2.0)

def approximate(operation): 
    def approximater(vec):
        x, y = vec
        return vec2(operation(x), operation(y))
    return approximater

rnd = approximate(round)
clg = approximate(math.ceil)
flr = approximate(math.floor)
