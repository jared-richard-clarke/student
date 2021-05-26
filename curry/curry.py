# Python 3
def add(x):
    return lambda y: x + y

add_1 = add(1)
seven = add_1(6)

eleven = add(1)(10)
