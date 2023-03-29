// fix(value) -> immutable value
// Copies objects and freezes them with a null prototype.
// Copies and freezes arrays but preserves prototype.
// Copies primitive values.
// Recursively copies and freezes nested objects and arrays.

function fix(value) {
    if (Array.isArray(value)) {
        return Object.freeze(value.map((child) => fix(child)));
    }

    if (typeof value === "object" && value !== null) {
        return Object.freeze(
            Object.entries(value).reduce((accum, [key, value]) => {
                accum[key] = fix(value);
                return accum;
            }, Object.create(null))
        );
    }

    return value;
}
