// module op: provides a set of variable-argument, arithmetic functions.
// op.add(1, 2, op.sub(8, 4)) -> 7

function operator(operation, base) {
    return function (...numbers) {
        if (numbers.length === 0) {
            return base;
        } else {
            return numbers.reduce((accum, number) => operation(accum, number));
        }
    };
}

const op = Object.create(null);

op.add = operator((x, y) => x + y, 0);
op.sub = operator((x, y) => x - y, 0);
op.mul = operator((x, y) => x * y, 1);
op.div = operator((x, y) => x / y, 1);

export default Object.freeze(op);
