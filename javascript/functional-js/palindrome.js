// === functions ===
const lowercase = (text) => text.toLowerCase();
const strip_whitespace = (text) => text.replace(/\s+/g, "");
const strip_punctuation = (text) => text.replace(/[?!.]+/g, "");
const reverse = (text) => text.split("").reverse().join("");
const is_equal = (x, y) => x === y;

const pipe = function (...actions) {
    return function (input) {
        return actions.reduce((reduction, action) => action(reduction), input);
    };
};

// === composite functions ===
const clean = pipe(lowercase, strip_whitespace, strip_punctuation);

const is_palindrome = function (corpus) {
    return is_equal(clean(corpus), reverse(clean(corpus)));
};
