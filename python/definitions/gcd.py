# === Euclid's Algorithm for Greatest Common Divisors ===
# GCD(integer, integer) -> integer
# The greatest common divisor of x and y is defined to be the largest
# integer that divides both x and y with no remainder.
# GCD(10 5) -> 5

def GCD(x, y):
    if type(x) != int or type(y) != int:
        raise TypeError("Arguments must be of type int")
    while not (y == 0):
        prev = x
        x = y
        y = prev % y
    else: 
        return x
