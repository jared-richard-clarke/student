# pipe(*functions) -> function
# Composes a series of value functions into a single value function.
# sums = pipe(lambda x: x + 1, lambda x: x * 10)
# sums(6) -> 70

def pipe(*actions):
    def reducer(input):
        accum = input
        for action in actions:
            accum = action(accum)
        return accum
    return reducer
 
