# === fixed point ===
# fixed_point(function, number) -> number
# A number x is called a fixed point of a function ƒ if x satisfies  
# the equation ƒ(x) = x. For some functions ƒ we can locate a fixed point
# by beginning with an initial guess and applying ƒ repeatedly.

def fixed_point(func, firstguess):
    TOLERANCE = 0.00001
    def is_close(x, y): 
        return abs(x - y) < TOLERANCE
    def attempt(guess):
        next = func(guess)
        while True:
            if is_close(guess, next):
                return next
            guess = next
            next = func(next)
    return attempt(firstguess)

# === average-damp ===
# average_damp(function) -> function(number) -> number
# Average damping forces successive approximations to converge
# where they might otherwise loop infinitely.
def average_damp(func):
    def average(a, b): 
        return (a + b) / 2
    def damper(n): 
        return average(n, func(n))
    return damper


# === Newton's Method for approximating square roots ===
# √x = the y such that y ≥ 0 and y ^ 2 = x
# Average y with x ÷ y in successive approximations — each average
# closer than the last.
# guess        quotient                 average
# 1            (2 ÷ 1) = 2              ((2 + 1) ÷ 2) = 1.5
# 1.5          (2 ÷ 1.5) = 1.3333       ((1.3333 ÷ 1.5) ÷ 2) = 1.4167
# 1.4167 ...

def square_root(x):
    return fixed_point(average_damp(lambda y: x / y), 1.0)
