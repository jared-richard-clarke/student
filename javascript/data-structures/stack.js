// === stack object ===

// factory
function create_stack() {
    // stack instance, protected within closure
    const stack = [];
    // methods
    const m = Object.create(null);
    m.is_empty = function() {
        return stack.length === 0;
    };
    m.push = function(...values) {
        values.forEach((value) => stack.push(value));
    };
    m.peek = function() {
        return stack[stack.length - 1];
    };
    m.pop = function() {
        return stack.pop();
    };
    m.clear = function() {
        while (stack.length > 0) {
            stack.pop();
        }
    };
    // interface
    return Object.freeze(m);
}
