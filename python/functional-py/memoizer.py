"""
memoize (function) -> function -> value
Wraps function in a function that stores previously-computed values. Eliminates redundant computation.
Works only with pure functions. Mutable values cannot be cached.
const add = memoize(lambda x, y: x + y)
add(1, 6) -> caches then returns 7
"""
def memoize(func):
    cache = dict()
    def memoizer (*args):
        if args in cache:
            return cache[args]
        else:
            result = func(*args)
            cache[args] = result
            return result
    return memoizer
