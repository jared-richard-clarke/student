// module str: functional string manipulation.

const str = Object.create(null);

// append(string, string) -> string
// Joins two strings into one.
// append("hello" "world") -> "hello world"

str.append = function (str1, str2) {
    return str1 + str2;
};

// length(string) -> number
// Returns the length of a string.
// length("hello") -> 5

str.length = function (str) {
    return str.length;
};

// map(function, string) -> string
// Maps a function over each character in a string.
// map((x) => append(x, "-"), "hello") -> "h-e-l-l-o-"

str.map = function (fn, str) {
    return [...str].map(fn).join("");
};

// filter(function, string) -> string
// Returns a string whose characters satisfy the predicate function.
// filter((x) => x === "l", "hello") -> "ll"

str.filter = function (fn, str) {
    return [...str].filter(fn).join("");
};

// reverse(string) -> string
// Reverses the order of characters in a string.
// reverse("hello") -> "olleh"

str.reverse = function (str) {
    return [...str].reverse().join("");
};

// split(string, string) -> array
// Splits a string into an array strings, divided by a separator string.
// Default separator is an empty string.
// split("hello") -> ["h", "e", "l", "l", "o"]

str.split = function (str, sep = "") {
    return sep === "" ? [...str] : str.split(sep);
};

// join([string], string) -> string
// Joins an array of strings into a single string via a separator string.
// Default separator is an empty string.
// join(["h", "e", "l", "l", "o"]) -> "hello"

str.join = function (strs, sep = "") {
    return strs.join(sep);
};

// slice(string, number, number) -> string
// Returns a substring given a start and end index.
// slice("hello", 0, 2) -> "he"

str.slice = function (str, start, end) {
    return str.slice(start, end);
};

// trim(string) -> string
// Removes whitespace from the start and end of a string.
// trim("  hello  ") -> "hello"

str.trim = function (str) {
    return str.trim();
};

// trim_start(string) -> string
// Removes whitespace from the start of a string.
// trim_start("  hello") -> "hello"

str.trim_start = function (str) {
    return str.replace(/^\s+/, "");
};

// trim_end(string) -> string
// Removes whitespace from the end of a string.
// trim_end("hello  ") -> "hello"

str.trim_end = function (str) {
    return str.replace(/\s+$/, "");
};

// words(string) -> [string]
// Splits a string into an array of strings delimited by whitespace.
// words("hello world") -> ["hello", "world"]

str.words = function (str) {
    return str.trim().split(/\s+/g);
};

// lines(string) -> [string]
// Splits a string into an array of strings delimited by line breaks.
// lines("hello\nworld") -> ["hello", "world"]

str.lines = function (str) {
    return str.split(/\r\n|\r|\n/g);
};

// up_case(string) -> string
// Converts all characters to uppercase.
// up_case("hello") -> "HELLO"

str.up_case = function (str) {
    return str.toUpperCase();
};

// down_case(string) -> string
// Converts all characters to lowercase.
// down_case("HELLO") -> "hello"

str.down_case = function (str) {
    return str.toLowerCase();
};

// all(function, string) -> boolean
// Determines whether all characters satisfy the given predicate function.
// all((x) => x === "l", "hello") -> false

str.all = function (fn, str) {
    return [...str].every(fn);
};

// any(function, string) -> boolean
// Determines whether some characters satisfy the given predicate function.
// any((x) => x === "l", "hello") -> true

str.any = function (fn, str) {
    return [...str].some(fn);
};

// from_codepoint(number) -> string
// Pulled from https://github.com/acornjs/acorn/blob/master/acorn/src/util.js
// Converts code point to UTF-16 encoded string.
// from_codepoint(0x0041) -> "A"

str.from_codepoint = function (code) {
    // UTF-16 Decoding
    if (code <= 0xffff) return String.fromCharCode(code);
    code -= 0x10000;
    return String.fromCharCode((code >> 10) + 0xd800, (code & 1023) + 0xdc00);
};


export default Object.freeze(str);
