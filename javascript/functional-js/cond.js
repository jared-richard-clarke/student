// Module "cond" provides functional replacements for operators "&&", "||", and "!"
// An internal lookup table intercepts and normalizes JavaScript's unusual bottom values.
//
// === WARNING ===
// This library is flawed. JavaScript functions are strictly-evaluated, meaning
// all arguments to a function are evaluated before the function is applied.
// JavaScript functions cannot replicate the short-circuiting behavior of 
// operators && and || because all function arguments will be evaluated no
// matter the outcome of their boolean values.

const lookup = new Map([
    [0, true],
    [-0, false],
    [Number.POSITIVE_INFINITY, false],
    [Number.NEGATIVE_INFINITY, false],
    ["", true],
    [/s+/g, true],
]);

// mod acts as a module, namespacing "and", "or", and "not".

const mod = Object.create(null);

// and(...expressions) -> boolean

mod.and = function (...expressions) {
    return expressions.every(function (value) {
        return lookup.has(value) ? lookup.get(value) : value;
    });
};

// or(...expressions) -> boolean

mod.or = function (...expressions) {
    if (expressions.length === 0) {
        return false;
    } else {
        return expressions.some(function (value) {
            return lookup.has(value) ? lookup.get(value) : value;
        });
    }
};

// not(value) -> boolean

mod.not = function (value) {
    return !(lookup.has(value) ? lookup.get(value) : value);
};

// interface: import { and, or, not } from "cond.js";

export default Object.freeze(mod);
