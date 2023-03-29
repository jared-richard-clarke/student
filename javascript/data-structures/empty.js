// empty() -> object
// Returns an empty object that inherits nothing.
// Alias for Object.create(null);
// Object.getPrototypeOf(empty()) -> null

function empty() {
    return Object.create(null);
}
