# first iteration
import re

def type_check(TYPE):
    def decorate(action):
        def call(value):
            if type(value) != TYPE:
                return value
            else:
                return action(value)
        return call
    return decorate

@type_check(str)
def trim_whitespace(text):
    def outer(text):
        return re.sub(r"^\s+|\s+$", "", text)
    def inner(text):
        return re.sub(r"\s+", " ", text)
    def punct(text):
        return re.sub(r"\s+(?=[.,;:?!])", "", text)
    return outer(inner(punct(text)))


result = trim_whitespace("  this,   that, and the other .")
# returns "this, that, and the other."
