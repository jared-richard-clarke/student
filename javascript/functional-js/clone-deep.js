// function pulled from the Tailwind framework.

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
