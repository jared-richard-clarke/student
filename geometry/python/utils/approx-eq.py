# EPSILON
# The maximum allowable difference in precision between floating-point numbers.

EPSILON = 0.000001

# approx_eq(number, number) -> boolean
# Tests for approximate equality between two floating-point numbers within an absolute
# or relative tolerance of EPSILON. An absolute tolerance is used for values
# less than or equal to 1.0. A relative tolerance is used for larger values.
# approx_eq(0.2, 0.19999999) -> True

def approx_eq(x, y):
    return abs(x - y) <= (EPSILON * max(1.0, abs(x), abs(y)))
