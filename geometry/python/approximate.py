EPSILON = 0.000001
"""The arbitrary maximum allowable difference in precision between floating-point numbers."""


def equals(x, y):
    """
    Tests for approximate equality between two floating-point numbers within an absolute
    or relative tolerance of EPSILON. An absolute tolerance is used for values
    less than or equal to 1.0. A relative tolerance is used for larger values.
    """
    return abs(x - y) <= (EPSILON * max(1.0, abs(x), abs(y)))
