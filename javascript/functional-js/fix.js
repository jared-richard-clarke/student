// fix(Object) -> Object: immutable, null prototype
// Takes an object literal and returns a new object with the same
// key-value pairs but is immutable and inherits null.

function fix(obj) {
    return Object.freeze(
        Object.entries(obj).reduce((accum, [key, value]) => {
            accum[key] = value;
            return accum;
        }, Object.create(null))
    );
}
