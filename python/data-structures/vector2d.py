import math

class Vector2D:
    def __init__(self, x, y):
        self.x = x
        self.y = y
        self.point = (x, y)
        self.magnitude = math.hypot(x, y)

    def unit_vector(self):
        x, y = self.point
        mag = self.magnitude
        return (x / mag, y / mag)
        
def scale(v, factor):
    x, y = v.point
    return Vector2D((x * factor), (y * factor))

def add(v1, v2):
    x1, y1 = v1.point
    x2, y2 = v2.point
    return Vector2D(x1 + x2, y1 + y2)

def dot_product(v1, v2):
    x1, y1 = v1.point
    x2, y2 = v2.point
    return (x1 * x2) + (y1 * y2)

def cross_product(v1, v2):
    x1, y1 = v1.point
    x2, y2 = v2.point
    return (x1 * y2) - (y1 * x2)

def compare(operation):
    def operator(v1, v2):
        m1 = v1.magnitude
        m2 = v2.magnitude
        return operation(m1, m2)
    return operator

gt = compare(lambda x, y: x > y)
lt = compare(lambda x, y: x < y)
eq = compare(lambda x, y: x == y)
