// === stack object ===

// factory
const create_stack = function () {
    // object instance
    const stack = [];
    // methods
    const is_empty = function () {
        return stack.length === 0;
    };
    const push = function (...values) {
        values.forEach(function (value) {
            stack.push(value);
        });
    };
    const peek = function () {
        return stack[stack.length - 1];
    };
    const pop = function () {
        return stack.pop();
    };
    // interface
    return Object.freeze({
        is_empty,
        push,
        peek,
        pop,
    });
};

// === example ===
// const stack = create_stack();
// stack.is_empty(); -> true
// stack.push(1, 2, 3);
// stack.peek(); -> 3
// stack.pop();
// stack.peek(); -> 2
