const lowercase = function (text) {
    return text.toLowerCase();
};
const strip_whitespace = function (text) {
    // whitespace before tab deliberate.
    return text.replace(/[ \t\n\r\v\f]/g, "");
};
const strip_punctuation = function (text) {
    return text.replace(/[!"#$%&'()*+,-./:;<=>?@[\]^_`{|}~]/g, "");
};
const reverse = function (text) {
    return text.split("").reverse().join("");
};
const is_equal = function (x, y) {
    return x === y;
};
const pipe = function (...actions) {
    return function (input) {
        return actions.reduce(function (accum, action) {
            return action(accum);
        }, input);
    };
};
// composite function
const clean = pipe(lowercase, strip_whitespace, strip_punctuation);

// is_palindrome(string) -> boolean
// Check whether text is spelled the same backwards and forwards.
// is_palindrome("Anna") -> true
const is_palindrome = function (corpus) {
    const text = clean(corpus);
    const txet = reverse(text);
    return is_equal(text, txet);
};
