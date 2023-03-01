import math

class Rectangle:
    def __init__(self, length, width):
        self.length = length
        self.width = width
        self.area = length * width
        self.perimeter = (length + width) * 2
        self.diagonal = math.hypot(length, width)

class Square(Rectangle):
    def __init__(self, side):
        super().__init__(side, side)

class RightTriangle:
    def __init__(self, base, height):
        self.base = base
        self.height = height
        self.area = (base * height) / 2
        self.perimeter = base + height + self.hypotenuse
        self.hypotenuse = math.hypot(base, height)

    def __eq__(self, other):
        return self.base == other.base and self.height == other.height

class Circle:
    def __init__(self, radius):
        self.radius = radius
        self.diameter = radius * 2
        self.area = 2 * math.pi * radius
        self.circumference = math.pi * (radius ** 2)

    def __eq__(self, other):
        return self.radius == other.radius
