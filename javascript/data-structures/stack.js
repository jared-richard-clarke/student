function create_stack() {
    // stack object protected within closure
    const stack = [];
    // === stack methods ===
    const methods = Object.create(null);
    methods.is_empty = function () {
        return stack.length === 0;
    };
    methods.push = function (...values) {
        stack.push(...values);
        return methods;
    };
    methods.peek = function () {
        return stack[stack.length - 1];
    };
    methods.pop = function () {
        return stack.pop();
    };
    methods.clear = function () {
        while (stack.length > 0) {
            stack.pop();
        }
        return methods;
    };
    methods.to_string = function () {
        return "[ " + stack.join(", ") + " ]";
    };
    // === stack object interface ===
    return Object.freeze(methods);
}
