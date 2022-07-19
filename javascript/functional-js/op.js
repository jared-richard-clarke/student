// module op: provides a set of variable-argument, arithmetic functions.
// op.add(1, 2, op.sub(8, 4)) -> 7
// op.mul() -> 1

function operator(operation, identity) {
    return function (...operands) {
        if (operands.length === 0) {
            return identity;
        } else {
            return operands.reduce(
                (total, operand) => operation(total, operand)
            );
        }
    };
}

const op = Object.create(null);

op.add = operator((x, y) => x + y, 0);
op.sub = operator((x, y) => x - y, 0);
op.mul = operator((x, y) => x * y, 1);
op.div = operator((x, y) => x / y, 1);

export default Object.freeze(op);
