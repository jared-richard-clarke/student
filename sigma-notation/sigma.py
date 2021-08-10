# Python 3
def sigma(operation, start, stop):
    if start > stop:
        return 0
    sum = 0
    for value in range(start, stop + 1):
        sum += operation(value)
    return sum

# result = 30
result = sigma(lambda x: x * x, 1, 4)
