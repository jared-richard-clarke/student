// enumerate(...values) -> Object { string: value }
// Takes a variable number of values and returns an immutable, enumerated object
// where the keys are stringified versions of the values they point at.
// enumerate("cat", 1, true) -> { "cat": "cat", "1": 1, "true": true }

function enumerate(...values) {
    return Object.freeze(
        values.reduce(function (accum, value) {
            accum[value] = value;
            return accum;
        }, Object.create(null))
    );
}
