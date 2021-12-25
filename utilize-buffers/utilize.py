import sys
import os
import io
import re

def create():
    """Compose replace function."""
    # lookup table for 'utilize' and replacements
    dictionary = {
        "utilize": "use",
        "utilise": "use",
        "Utilize": "Use",
        "Utilise": "Use",
        "utilizes": "uses",
        "utilises": "uses",
        "utilized": "used",
        "utilised": "used",
        "utilizing": "using",
        "utilising": "using",
        "Utilizing": "Using",
        "Utilising": "Using"
    }
    # helper functions in regular expressions take match objects, not strings
    match = lambda mo: dictionary[mo.group(0)]
    # compile regular expression for repeated use
    re_object = re.compile(r"[uU]tili([zs]e|[zs]ed|[zs]ing)")
    # composite function
    def replacer(text):
        if type(text) != str:
            raise TypeError("argument must be of type string")
        else:
            return re_object.sub(match, text)
    # return composited function for use
    return replacer

replace = create()
"""Scan string literal, replacing 'utilize' and its variants with 'use' and its variants."""

# 1. collect user input
if __name__ == "__main__":
    file = input("Input .txt file: ")
    root, ext = os.path.splitext(file)
    if not os.path.exists(file):
        print("file does not exist")
        sys.exit(1)
    if not ext == ".txt":
        print("file must have .txt extension")
        sys.exit(1)
# 2. open files
    with io.open(file, buffering=1, encoding="utf-8") as text:
        print(replace(text.read()))
