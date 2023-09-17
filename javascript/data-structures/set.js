const set = Object.freeze({
    create: function (xs) {
        const data = Object.create(null);
        xs.forEach((x) => (data[x] = true));
        return Object.freeze(data);
    },
    has: function (set, x) {
        return set[x] === true;
    },
    unite: function (x, y) {
        const data = Object.create(null);
        Object.keys(x).forEach((key) => (data[key] = true));
        Object.keys(y).forEach((key) => (data[key] = true));
        return Object.freeze(data);
    },
    intersect: function (x, y) {
        const data = Object.create(null);
        Object.keys(x).forEach((key) => (data[key] = y[key]));
        return Object.freeze(data);
    },
    print: function (x) {
        return "[ " + Object.keys(x).join(", ") + " ]";
    },
});
