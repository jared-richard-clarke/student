import unittest
from matrices import Mat3
from approximate import EPSILON

ID = Mat3(1, 0, 0, 1, 0, 0)


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
        expect = Mat3(0.2836621854632263, -0.9589242746631385,
                      0.9589242746631385, 0.2836621854632263, 0.0, 0.0,)
        result = ID.rotate(5)
        self.assertEqual(expect, result)

    def test_shear(self):
        expect = Mat3(1, 4, 3, 1, 0, 0)
        result = ID.shear(3, 4)
        self.assertEqual(expect, result)

    def test_transform(self):
        expect = Mat3(2, 4, 2, 2, 3, 4)
        result = ID.translate(3, 4).scale(2, 2).shear(1, 2)
        self.assertEqual(expect, result)


if __name__ == "__main__":
    unittest.main()
