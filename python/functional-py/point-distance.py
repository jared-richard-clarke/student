from math import sqrt

# distance(tuple, tuple) -> float
# Calculates the distance between two points.
# distance((3, 0), (2, 0)) -> 1.0
def distance(px, py):
    x1, x2 = px 
    y1, y2 = py
    hypotenuse = lambda x, y: sqrt((x * x) + (y * y))
    return hypotenuse(x1 - y1, x2 - y2)
