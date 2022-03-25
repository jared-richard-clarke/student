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
        
def scale(v, factor):
    if type(v) != Vector:
        raise TypeError("First argument must be type Vector.")
    x = v.x
    y = v.y
    return Vector((x * factor), (y * factor))
