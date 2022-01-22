// sum(...number) -> number
// returns the sum of variable number arguments, adding pairwise from left to right.
// sum(1, 2, 4) -> 7

// === ES6 ===
function sum_es6(...numbers) {
    return numbers.reduce((result, number) => result + number);
}

// === ES5 ===
("use strict");
function sum_es5() {
    var numbers = [];
    for (var i = 0; i < arguments.length; i += 1) {
        numbers[i] = arguments[i];
    }
    return numbers.reduce(function (result, number) { return result + number; });
}
