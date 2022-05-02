import math

# point(number, number) -> tuple
# Constructs a two dimensional point as a tuple.
# point(1, 2) -> (1, 2)

def point(x, y):
    return (x, y)

# ORIGIN: the point of origin in a two-dimensional coordinate system.

ORIGIN = point(0, 0)

# segment(tuple, tuple) -> float
# Calculates the distance between two points.
# segment((3, 0), (2, 0)) -> 1.0

def segment(p1, p2):
    x1, y1 = p1 
    x2, y2 = p2
    return math.hypot(x2 - x1, y2 - y1)

# path(tuple) -> tuple
# Constructs a path as a tuple of points.
# path(point(1, 2), point(3, 4)) -> ((1, 2), (3, 4))

def path(*points):
    return points

# path_length(tuple) -> float
# Computes the length of a path along a series of points.
# path_length(path(point(1, 1), point(5, 1), point(5, 4), point(1, 1))) -> 12.0

def path_length(path):
    sum = 0
    length = len(path)
    for i in range(length - 1):
        p1 = path[i]
        p2 = path[i + 1]
        sum += segment(p1, p2)
    return sum

# approximate(function) -> function(point) -> point
# Generates approximation functions for simplifying point components.
# point_round = approximate(round) -> point_round(point(1.3, 1.7)) -> (1.0, 2.0)

def approximate(operation): 
    def approximater(pt):
        x, y = pt
        return point(operation(x), operation(y))
    return approximater

point_round = approximate(round)
point_ceiling = approximate(math.ceil)
point_floor = approximate(math.floor)
