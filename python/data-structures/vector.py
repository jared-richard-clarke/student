import math

class Vector:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    @staticmethod
    def is_vector(data):
        return type(data) == Vector

    def point(self):
        return (self.x, self.y)

    def magnitude(self):
        return math.hypot(self.x, self.y)

    def unit_vector(self):
        mag = self.magnitude()
        return (self.x / mag, self.y / mag)
        
def scale(v, factor):
    if type(v) != Vector:
        raise TypeError("First argument must be type Vector.")
    x = v.x
    y = v.y
    return Vector((x * factor), (y * factor))

def add(v1, v2):
    if type(v1) != Vector and type(v2) != Vector:
        raise TypeError("Both arguments must be type Vector.")
    x1, y1 = v1.x, v1.y
    x2, y2 = v2.x, v2.y
    return Vector(x1 + x2, y1 + y2)
