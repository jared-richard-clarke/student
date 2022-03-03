# type_check(type, *function) -> *function
# Inputs a type and variadic function and returns 
# a variadic function that checks the types of its inputs.

# def add(*numbers):
#     sum = 0
#     for number in numbers:
#         sum += number
#     return sum

# add = type_check(int, add)
# add(1, 6) -> 7

def type_check(check, action):
    def wrapper(*inputs):
        for input in inputs:
            if type(input) != check:
                raise TypeError(f"{input} must be of type {check}")
        return action(*inputs)
    return wrapper
