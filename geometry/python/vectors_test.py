import unittest
from vectors import Vec2, Vec3
from approximate import EPSILON


class TestMatrices(unittest.TestCase):
    def test_eq(self):
        self.assertTrue(Vec2(3, 4) == Vec2(3, 4))

    def test_approx_eq(self):
        ep = EPSILON
        # Vec2
        result = Vec2(3, 4).approx_eq(Vec2(3 + ep, 4 + ep))
        self.assertTrue(result)
        # Vec3
        result = Vec3(3, 4, 5).approx_eq(Vec3(3 + ep, 4, 5 + ep))
        self.assertTrue(result)

    def test_add(self):
        # Vec2
        expect = Vec2(4, 6)
        result = Vec2(1, 2) + Vec2(3, 4)
        self.assertEqual(expect, result)
        # Vec3
        expect = Vec3(4, 6, 2)
        result = Vec3(1, 2, 1) + Vec3(3, 4, 1)
        self.assertEqual(expect, result)

    def test_sub(self):
        # Vec2
        expect = Vec2(2, 2)
        result = Vec2(3, 4) - Vec2(1, 2)
        self.assertEqual(expect, result)
        # Vec3
        expect = Vec3(2, 2, 2)
        result = Vec3(3, 4, 5) - Vec3(1, 2, 3)
        self.assertEqual(expect, result)

    def test_negate(self):
        # Vec2
        expect = Vec2(-3, -4)
        result = -Vec2(3, 4)
        self.assertEqual(expect, result)
        # Vec3
        expect = Vec3(-3, -4, -5)
        result = -Vec3(3, 4, 5)
        self.assertEqual(expect, result)

    def test_invert(self):
        # Vec2
        expect = Vec2(0.5, 0.5)
        result = Vec2(2, 2).invert()
        self.assertEqual(expect, result)
        # Vec3
        expect = Vec3(0.5, 0.5, 0.5)
        result = Vec3(2, 2, 2).invert()
        self.assertEqual(expect, result)

    def test_magnitude(self):
        # Vec2
        expect = 5
        result = Vec2(3, 4).mag()
        self.assertEqual(expect, result)
        # Vec3
        expect = 7.0710678118654755
        result = Vec3(3, 4, 5).mag()
        self.assertEqual(expect, result)

    def test_scale(self):
        # Vec2
        expect = Vec2(33, 44)
        result = Vec2(3, 4).scale(11)
        self.assertEqual(expect, result)
        # Vec3
        expect = Vec3(33, 44, 55)
        result = Vec3(3, 4, 5).scale(11)
        self.assertEqual(expect, result)

    def test_dot_product(self):
        # Vec2
        expect = 11
        result = Vec2(3, 4).dot(Vec2(1, 2))
        self.assertEqual(expect, result)
        # Vec3
        expect = 26
        result = Vec3(3, 4, 5).dot(Vec3(1, 2, 3))
        self.assertEqual(expect, result)

    def test_distance(self):
        # Vec2
        expect = 7
        result = Vec2(8, 0).distance(Vec2(1, 0))
        self.assertEqual(expect, result)
        # Vec3
        expect = 7
        result = Vec3(10, 0, 0).distance(Vec3(3, 0, 0))

    def test_normalize(self):
        # Vec2
        expect = Vec2(0.6, 0.8)
        result = Vec2(3, 4).normalize()
        self.assertEqual(expect, result)
        # Vec3
        expect = Vec3(0.5883484054145521, 0.7844645405527362,
                      0.19611613513818404)
        result = Vec3(3, 4, 1).normalize()
        self.assertEqual(expect, result)

    def test_round(self):
        # Vec2
        expect = Vec2(1, 4)
        result = Vec2(0.75, 4.25).round()
        self.assertEqual(expect, result)
        # Vec3
        expect = Vec3(1, 4, 1)
        result = Vec3(0.75, 4.25, 1.15).round()
        self.assertEqual(expect, result)


if __name__ == "__main__":
    unittest.main()
