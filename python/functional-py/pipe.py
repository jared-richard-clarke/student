# compose(*functions) -> function
# Composes a series of single-argument functions into a one function.
# sums = compose(lambda x: x + 1, lambda x: x * 10)
# sums(6) -> 70

def compose(*actions):
    def reducer(input):
        accum = input
        for action in actions:
            accum = action(accum)
        return accum
    return reducer
 
# pipe(value, *function) -> value
# Transforms a value through a series of single-argument functions.
# pipe(6, lambda x: x + 1, lambda x: x * 10) -> 70

def pipe(input, *actions):
    accum = input
    for action in actions:
        accum = action(accum)
    return accum
