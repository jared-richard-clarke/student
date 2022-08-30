import unittest
from vectors import Vec2
from approximate import EPSILON


class TestMatrices(unittest.TestCase):
    def test_eq(self):
        self.assertTrue(Vec2(3, 4) == Vec2(3, 4))

    def test_approx_eq(self):
        ep = EPSILON
        result = Vec2(3, 4).approx_eq(Vec2(3 + ep, 4 + ep))
        self.assertTrue(result)

    def test_add(self):
        expect = Vec2(4, 6)
        result = Vec2(1, 2) + Vec2(3, 4)
        self.assertEqual(expect, result)

    def test_sub(self):
        expect = Vec2(2, 2)
        result = Vec2(3, 4) - Vec2(1, 2)
        self.assertEqual(expect, result)

    def test_negate(self):
        expect = Vec2(-3, -4)
        result = -Vec2(3, 4)
        self.assertEqual(expect, result)

    def test_magnitude(self):
        expect = 5
        result = Vec2(3, 4).mag()
        self.assertEqual(expect, result)

    def test_scale(self):
        expect = Vec2(33, 44)
        result = Vec2(3, 4).scale(11)
        self.assertEqual(expect, result)

    def test_dot_product(self):
        expect = 11
        result = Vec2(3, 4).dot(Vec2(1, 2))
        self.assertEqual(expect, result)

    def test_distance(self):
        expect = 7
        result = Vec2(8, 0).distance(Vec2(1, 0))
        self.assertEqual(expect, result)

    def test_lerp(self):
        expect = Vec2(5, 0)
        result = Vec2(0, 0).lerp(Vec2(10, 0), 0.5)
        self.assertEqual(expect, result)

    def test_normalize(self):
        expect = Vec2(0.6, 0.8)
        result = Vec2(3, 4).normalize()
        self.assertEqual(expect, result)

    def test_round(self):
        expect = Vec2(1, 4)
        result = Vec2(0.75, 4.25).round()
        self.assertEqual(expect, result)


if __name__ == "__main__":
    unittest.main()
