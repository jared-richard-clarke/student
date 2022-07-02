// empty() -> object
// Returns an empty object that inherits nothing.
// Object.getPrototypeOf(empty()) -> null

function empty() {
    return Object.create(null);
}
