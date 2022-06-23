import math


class Mat3:
    def __init__(self, xx, yx, xy, yy, x0, y0):
        self.xx = xx
        self.yx = yx
        self.xy = xy
        self.yy = yy
        self.x0 = x0
        self.y0 = y0

    def __str__(self):
        return f"({self.xx}, {self.yx}, {self.xy}, {self.yy}, {self.x0}, {self.y0})"

    def multiply(self, other):
        return Mat3(self.xx * other.xx + self.yx * other.xy,
                    self.xx * other.yx + self.yx * other.yy,
                    self.xy * other.xx + self.yy * other.xy,
                    self.xy * other.yx + self.yy * other.yy,
                    self.x0 * other.xx + self.y0 * other.xy + other.x0,
                    self.x0 * other.yx + self.y0 * other.yy + other.y0)

    def transform(self, *matrices):
        accum = self
        for matrix in matrices:
            accum = accum.multiply(matrix)
        return accum


def identity():
    return Mat3(1, 0,
                0, 1,
                0, 0)


def translate(x, y):
    return Mat3(1, 0,
                0, 1,
                x, y)


def scale(x, y):
    return Mat3(x, 0,
                0, y,
                0, 0)


def rotate(angle):
    c = math.cos(angle)
    s = math.sin(angle)
    return Mat3(c, s,
                -s, c,
                0, 0)


def shear(x, y):
    return Mat3(1, y,
                x, 1,
                0, 0)
