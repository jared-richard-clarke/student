# Python 3
def square(number):
    return number * number

result_one = list(map(square, [3, 4, 5]))
# returns [9, 16, 25]

# alternative
def map_list(action, numbers):
    return [action(number) for number in numbers]

result = map_list(lambda x: x * x, [1, 2, 3])
