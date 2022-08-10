// Provides arithmetic functions.
// module op = readonly {
//     1. add(number, number) -> number
//     2. sub(number, number) -> number
//     3. mul(number, number) -> number
//     4. div(number, number) -> number
//     5. sum(...number)      -> number
//     6. product(...number)  -> number
// }

function binary(operation) {
    return (x, y) => operation(x, y);
}

function monoid(operation, identity) {
    return (...operands) => {
        return operands.reduce(
            (total, operand) => operation(total, operand),
            identity
        );
    };
}

const op = Object.create(null);

op.add = binary((x, y) => x + y);
op.sub = binary((x, y) => x - y);
op.mul = binary((x, y) => x * y);
op.div = binary((x, y) => x / y);

op.sum = monoid((x, y) => x + y, 0);
op.product = monoid((x, y) => x * y, 1);

export default Object.freeze(op);
