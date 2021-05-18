# Python 3
def map_list(action, list):
    return [action(item) for item in list]

result = map_list(lambda number: number ** 2, [1, 2, 3])
# returns [1, 4, 9]
