# refactor
import re

def trim_whitespace(text):
    def outer(text):
        return re.sub(r"^\s+|\s+$", "", text)
    def inner(text):
        return re.sub(r"\s+", " ", text)
    def punct(text):
        return re.sub(r"\s+(?=[.,;:?!])", "", text)
    if type(text) != str:
        return text
    else:
        return outer(inner(punct(text)))

result = trim_whitespace("  this,   that, and the other .")
# returns "this, that, and the other."
