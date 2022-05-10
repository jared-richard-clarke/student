// module str: functional string manipulation.

export default Object.freeze({
    append,
    length,
    map,
    filter,
    reverse,
    split,
    join,
    slice,
    trim,
    trim_start,
    trim_end,
    words,
    lines,
    up_case,
    down_case,
    all,
    any,
});

// append(string, string) -> string
// Joins two strings into one.
// append("hello" "world") -> "hello world"

function append(str1, str2) {
    return str1 + str2;
}

// length(string) -> number
// Returns the length of a string.
// length("hello") -> 5

function length(str) {
    return str.length;
}

// map(function, string) -> string
// Maps a function over each character in a string.
// map((x) => append(x, "-"), "hello") -> "h-e-l-l-o-"

function map(fn, str) {
    return [...str].map(fn).join("");
}

// filter(function, string) -> string
// Returns a string whose characters satisfy the predicate function.
// filter((x) => x === "l", "hello") -> "ll"

function filter(fn, str) {
    return [...str].filter(fn).join("");
}

// reverse(string) -> string
// Reverses the order of characters in a string.
// reverse("hello") -> "olleh"

function reverse(str) {
    return [...str].reverse().join("");
}

// split(string, string) -> array
// Splits a string into an array strings, divided by a separator string.
// Default separator is an empty string.
// split("hello") -> ["h", "e", "l", "l", "o"]

function split(str, sep = "") {
    return sep === "" ? [...str] : str.split(sep);
}

// join([string], string) -> string
// Joins an array of strings into a single string via a separator string.
// Default separator is an empty string.
// join(["h", "e", "l", "l", "o"]) -> "hello"

function join(strs, sep = "") {
    return strs.join(sep);
}

// slice(string, number, number) -> string
// Returns a substring given a start and end index.
// slice("hello", 0, 2) -> "he"

function slice(str, start, end) {
    return str.slice(start, end);
}

// trim(string) -> string
// Removes whitespace from the start and end of a string.
// trim("  hello  ") -> "hello"

function trim(str) {
    return str.trim();
}

// trim_start(string) -> string
// Removes whitespace from the start of a string.
// trim_start("  hello") -> "hello"

function trim_start(str) {
    return str.replace(/^\s+/, "");
}

// trim_end(string) -> string
// Removes whitespace from the end of a string.
// trim_end("hello  ") -> "hello"

function trim_end(str) {
    return str.replace(/\s+$/, "");
}

// words(string) -> [string]
// Splits a string into an array of strings delimited by whitespace.
// words("hello world") -> ["hello", "world"]

function words(str) {
    return str.trim().split(/\s+/g);
}

// lines(string) -> [string]
// Splits a string into an array of strings delimited by line breaks.
// lines("hello\nworld") -> ["hello", "world"]

function lines(str) {
    return str.split(/\r\n|\r|\n/g);
}

// up_case(string) -> string
// Converts all characters to uppercase.
// up_case("hello") -> "HELLO"

function up_case(str) {
    return str.toUpperCase();
}

// down_case(string) -> string
// Converts all characters to lowercase.
// down_case("HELLO") -> "hello"

function down_case(str) {
    return str.toLowerCase();
}

// all(function, string) -> boolean
// Determines whether all characters satisfy the given predicate function.
// all((x) => x === "l", "hello") -> false

function all(fn, str) {
    return [...str].every(fn);
}

// any(function, string) -> boolean
// Determines whether some characters satisfy the given predicate function.
// any((x) => x === "l", "hello") -> true

function any(fn, str) {
    return [...str].some(fn);
}
