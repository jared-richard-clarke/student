# === fixed point ===
# fixed_point(function, number) -> number
# A number x is called a fixed point of a function ƒ if x satisfies  
# the equation ƒ(x) = x. For some functions ƒ we can locate a fixed point
# by beginning with an initial guess and applying ƒ repeatedly.

def square_root(number):
    tolerance = 0.00001
    guess = 1.0
    def approximate(g): 
        return (g + number / g) / 2
    next = approximate(guess)
    while abs(guess - next) >= tolerance:
        guess = next
        next = approximate(guess)
    else:
        return next
