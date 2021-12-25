import re

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

re_object = re.compile(r"[uU]tili([zs]e|[zs]ed|[zs]ing)")

# replacer functions in regular expressions take match objects, not strings
def match(mo):
    text = mo.group(0)
    return dictionary[text]


def replace_utilize(text):
    if type(text) != str:
        raise TypeError("argument must be of type string")
    else:
        return re_object.sub(match, text)
    
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
    with open(file, buffering=1, encoding="utf-8") as text:
        print(replace_utilize(text.read()))
