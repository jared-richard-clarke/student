# Adds variable number of arguments.
def add(*numbers):
    sum = 0
    for number in numbers:
        sum += number
    return sum
