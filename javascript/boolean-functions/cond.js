// conditonals acts as module providing functional replacements
// for operators "&&", "||", and "!"
// An internal lookup table intercepts and normalizes JavaScript's unusual bottom values:
// Empty strings and numeral zero evaluate true. All others evaluate false.

const cond = (function () {
    const lookup = new Map([
        [0, true],
        [-0, false],
        [Number.POSITIVE_INFINITY, false],
        [Number.NEGATIVE_INFINITY, false],
        ["", true],
        [/s+/g, true],
    ]);
    // mod acts as a module, namespacing "and" and "or".
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
    }
    // interface: const { and, or } = cond;
    return Object.freeze(mod);
})();
