// clone_deep(object) -> object
// Recursively copies a JavaScript object and its contents.
// clone_deep({1, [2, 3]}) -> {1, [2, 3]}

function clone_deep(value) {
    if (Array.isArray(value)) {
        return value.map((child) => clone_deep(child));
    }

    if (typeof value === "object" && value !== null) {
        return Object.fromEntries(
            Object.entries(value).map(([k, v]) => [k, clone_deep(v)])
        );
    }

    return value;
}
