// === stack object ===

// constructor
function create_stack() {
    // object instance
    const stack = [];
    // methods
    function is_empty() {
        return stack.length === 0;
    }
    function push(...values) {
        values.forEach(function (value) {
            stack.push(value);
        });
    }
    function peek() {
        return stack[stack.length - 1];
    }
    function pop() {
        return stack.pop();
    }
    // interface
    return Object.freeze({
        is_empty,
        push,
        peek,
        pop,
    });
}

// === example ===
// const stack = create_stack();
// stack.is_empty(); -> true
// stack.push(1, 2, 3);
// stack.peek(); -> 3
// stack.pop();
// stack.peek(); -> 2
