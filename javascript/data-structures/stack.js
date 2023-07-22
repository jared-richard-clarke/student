function make_stack() {
    // stack object, protected within closure
    const stack = [];
    // === stack methods ===
    const m = Object.create(null);
    m.is_empty = function () {
        return stack.length === 0;
    };
    m.push = function (...values) {
        values.forEach((value) => stack.push(value));
        return m;
    };
    m.peek = function () {
        return stack[stack.length - 1];
    };
    m.pop = function () {
        return stack.pop();
    };
    m.clear = function () {
        while (stack.length > 0) {
            stack.pop();
        }
        return m;
    };
    // === stack object interface ===
    return Object.freeze(m);
}
