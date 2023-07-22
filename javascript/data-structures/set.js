function make_set() {
    // === set object ===
    const set = {};
    // === set methods ===
    const m = Object.create(null);
    m.has = function (value) {
        return set[value];
    };
    m.add = function (...values) {
        values.forEach((value) => {
            if (!set[value]) {
                set[value] = true;
            }
        });
        return m;
    };
    m.remove = function (value) {
        if (set[value]) {
            delete set[value];
        }
        return m;
    };
    m.clear = function () {
        Object.keys(set).forEach((key) => delete set[key]);
        return m;
    };
    m.to_string = function () {
        return "{ " + Object.keys(set).join(", ") + " }";
    };
    // === interface ===
    return Object.freeze(m);
}
