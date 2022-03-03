#!/usr/bin/env python3
import sys, os, io, re, resource

def create():
    # convert to byte strings for fast file processing.
    dictionary = {
        b"utilize": b"use",
        b"utilise": b"use",
        b"Utilize": b"Use",
        b"Utilise": b"Use",
        b"utilizes": b"uses",
        b"utilises": b"uses",
        b"utilized": b"used",
        b"utilised": b"used",
        b"utilizing": b"using",
        b"utilising": b"using",
        b"Utilizing": b"Using",
        b"Utilising": b"Using"
    }
    # helper functions in regular expressions take match objects, not strings or bytes
    matcher = lambda mo: dictionary[mo.group(0)]

    pattern = re.compile(rb"[uU]tili(?:[zs]ed|[zs]e|[zs]ing)")

    def replacer(text):
        if type(text) != bytes:
            raise TypeError("argument must be of type bytes")
        else:
            return pattern.sub(matcher, text)

    return replacer

replace = create()
"""Scan byte string, replacing 'utilize' and its variants with 'use' and its variants."""

# 1. collect and validate user input
if __name__ == "__main__":

    file = input("Input <plain>.txt file: ")
    root, ext = os.path.splitext(file)

    if not os.path.exists(file):
        print(f"file {root} does not exist")
        sys.exit(1)

    if not ext == ".txt":
        print(f"file {root} must have '.txt' extension")
        sys.exit(1)

# 2. open files / buffer and transform data / close files
    with io.open(file, mode="rb") as text:
        with io.open("re-"+file, mode="wb") as newtext:
            n = newtext.write(replace(text.read()))
            print(f"{n} bytes written")
