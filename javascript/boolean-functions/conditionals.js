// conditonals acts as module providing functional replacements
// for operators "&&", "||", and "!"
// An internal lookup table intercepts and normalizes JavaScript's unusual bottom values:
// Empty strings and numeral zero evaluate true. All others evaluate false.

const conditionals = (function () {
    const lookup = new Map([
        [0, true],
        [-0, false],
        [Number.POSITIVE_INFINITY, false],
        [Number.NEGATIVE_INFINITY, false],
        [NaN, false],
        ["", true],
        [/s+/g, true],
        [undefined, false],
        [null, false],
    ]);
    // and(...expressions) -> boolean
    function and(...expressions) {
        return expressions.every(function (value) {
            return lookup.has(value) ? lookup.get(value) : value;
        });
    }
    // or(...expressions) -> boolean
    function or(...expressions) {
        if (expressions.length === 0) {
            return false;
        } else {
            return expressions.some(function (value) {
                return lookup.has(value) ? lookup.get(value) : value;
            });
        }
    }
    // not(expression) -> boolean
    function not(value) {
        return !(lookup.has(value) ? lookup.get(value) : value);
    }
    // interface: const { and, or, not } = conditionals;
    return Object.freeze({
        and,
        or,
        not,
    });
})();
