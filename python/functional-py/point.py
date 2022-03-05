from math import sqrt

# point(number, number) -> tuple
# Constructs a two dimensional point represented as a tuple.
# point(1, 2) -> (1, 2)
def point(x, y):
    return (x, y)

# distance(tuple, tuple) -> float
# Calculates the distance between two points.
# distance((3, 0), (2, 0)) -> 1.0
def distance(px, py):
    x1, x2 = px 
    y1, y2 = py
    hypotenuse = lambda x, y: sqrt((x * x) + (y * y))
    return hypotenuse(x1 - y1, x2 - y2)
