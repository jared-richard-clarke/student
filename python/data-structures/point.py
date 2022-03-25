from math import sqrt

# point(number, number) -> tuple
# Constructs a two dimensional point as a tuple.
# point(1, 2) -> (1, 2)

def point(x, y):
    return (x, y)

# distance(tuple, tuple) -> float
# Calculates the distance between two points.
# distance((3, 0), (2, 0)) -> 1.0

def distance(p1, p2):
    def hypotenuse(x, y):
        return sqrt((x * x) + (y * y))
    x1, y1 = p1 
    x2, y2 = p2
    return hypotenuse(x2 - x1, y2 - y1)

# path(points) -> tuple
# Constructs a path as a tuple of points.
# path(point(1, 2), point(3, 4)) -> ((1, 2), (3, 4))

def path(*points):
    return points
