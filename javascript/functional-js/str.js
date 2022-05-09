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
    trim_left,
    trim_right,
    words,
    lines,
    up_case,
    down_case,
    all,
    any,
});

function append(str1, str2) {
    return str1 + str2;
}

function length(str) {
    return str.length;
}

function map(fn, str) {
    return [...str].map(fn).join("");
}

function filter(fn, str) {
    return [...str].filter(fn).join("");
}

function reverse(str) {
    return [...str].reverse().join("");
}

function split(str, sep = "") {
    return sep === "" ? [...str] : str.split(sep);
}

function join(strs, sep = "") {
    return strs.join(sep);
}

function slice(str, start, end) {
    return str.slice(start, end);
}

function trim(str) {
    return str.trim();
}

function trim_left(str) {
    return str.replace(/^\s+/, "");
}

function trim_right(str) {
    return str.replace(/\s+$/, "");
}

function words(str) {
    return str.trim().split(/\s+/g);
}

function lines(str) {
    return str.split(/\r\n|\r|\n/g);
}

function up_case(str) {
    return str.toUpperCase();
}

function down_case(str) {
    return str.toLowerCase();
}

function all(fn, str) {
    return [...str].every(fn);
}

function any(fn, str) {
    return [...str].some(fn);
}
