// Provides arithmetic functions.
// module op = readonly {
//     1. neg(number) ---------> number
//     2. add(number, number) -> number
//     3. sub(number, number) -> number
//     4. mul(number, number) -> number
//     5. div(number, number) -> number
//     6. exp(number, number) -> number
//     7. rem(number, number) -> number
//     8. sum(...number) ------> number
//     9. product(...number) --> number
// }

function unary(operation) {
    return Object.freeze(function (x) {
        return operation(x);
    });
}

function binary(operation) {
    return Object.freeze(function (x, y) {
        return operation(x, y);
    });
}

// A monoid is a set that is closed under a binary operation and has an identity element.
// Computationally, a monoid is an aggregation pattern.
function monoid(operation, identity) {
    return Object.freeze(function (...operands) {
        return operands.reduce(
            (total, operand) => operation(total, operand),
            identity
        );
    });
}

const op = Object.create(null);

op.neg = unary((x) => -x);

op.add = binary((x, y) => x + y);
op.sub = binary((x, y) => x - y);
op.mul = binary((x, y) => x * y);
op.div = binary((x, y) => x / y);
op.exp = binary((x, y) => x ** y);
op.rem = binary((x, y) => x % y);

op.sum = monoid((x, y) => x + y, 0);
op.product = monoid((x, y) => x * y, 1);

export default Object.freeze(op);
