const set = Object.freeze({
    create: function (xs) {
        const zs = Object.create(null);
        xs.forEach((x) => (zs[x] = true));
        return Object.freeze(zs);
    },
    // x ∈ xs
    has: function (xs, x) {
        return xs[x] === true;
    },
    size: function (xs) {
        return Object.keys(xs).reduce((accum, x) => accum + 1, 0);
    },
    // xs ∪ ys
    union: function (xs, ys) {
        const zs = Object.create(null);
        Object.keys(xs).forEach((x) => (zs[x] = true));
        Object.keys(ys).forEach((y) => (zs[y] = true));
        return Object.freeze(zs);
    },
    // xs \ ys
    difference: function (xs, ys) {
        const zs = Object.create(null);
        Object.keys(xs).forEach((x) => {
            if (ys[x] !== true) {
                zs[x] = xs[x];
            }
        });
        return Object.freeze(zs);
    },
    print: function (xs) {
        return "[ " + Object.keys(xs).join(", ") + " ]";
    },
});
