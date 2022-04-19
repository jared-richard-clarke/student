import math

# point(number, number) -> tuple
# Constructs a two dimensional point as a tuple.
# point(1, 2) -> (1, 2)

def point(x, y):
    return (x, y)

# distance(tuple, tuple) -> float
# Calculates the distance between two points.
# distance((3, 0), (2, 0)) -> 1.0

def distance(p1, p2):
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
        sum += distance(p1, p2)
    return sum

