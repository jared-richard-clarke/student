import unittest
import math
from matrices import Mat3
from utils import EPSILON
from vectors import Vec2

ID = Mat3.identity()


class TestMatrices(unittest.TestCase):
    def test_eq(self):
        self.assertTrue(Mat3(1, 2, 3, 4, 0, 0) == Mat3(1, 2, 3, 4, 0, 0))

    def test_approx_eq(self):
        ep = EPSILON
        expect = Mat3(1 + ep, 0, 0, 1 + ep, 0, 0)
        result = ID.approx_eq(expect)
        self.assertTrue(result)

    def test_translate(self):
        expect = Mat3(1, 0, 0, 1, 3, 4)
        result = ID.translate(3, 4)
        self.assertEqual(expect, result)

    def test_scale(self):
        expect = Mat3(3, 0, 0, 4, 0, 0)
        result = ID.scale(3, 4)
        self.assertEqual(expect, result)

    def test_rotate(self):
        expect = ID
        result = ID.rotate(math.radians(90)).rotate(math.radians(-90))
        self.assertEqual(expect, result)

    def test_shear(self):
        expect = Mat3(1, 4, 3, 1, 0, 0)
        result = ID.shear(3, 4)
        self.assertEqual(expect, result)

    def test_transform(self):
        expect = Mat3(2, 4, 2, 2, 3, 4)
        result = ID.translate(3, 4).scale(2, 2).shear(1, 2)
        self.assertEqual(expect, result)

    def test_transform_vector(self):
        expect = Vec2(6, 8)
        result = ID.scale(2, 2).transform_vector(Vec2(3, 4))
        self.assertEqual(expect, result)


if __name__ == "__main__":
    unittest.main()
