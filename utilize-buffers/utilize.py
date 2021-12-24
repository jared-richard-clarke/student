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
    return re_object.sub(match, text)
