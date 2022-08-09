// module "op" provides sum and product: variable-arity, arithmetic functions.
// op.sum(1, 2, 3, 4, 5, 6, 7, 8, 9, op.product(2, 5)) -> 55
// op.product() -> 1
// op.sum() -> 0

function monoid(operation, identity) {
    return Object.freeze(function (...operands) {
        return operands.reduce(
            (total, operand) => operation(total, operand),
            identity
        );
    });
}

const op = Object.create(null);

op.sum = monoid((x, y) => x + y, 0);
op.product = monoid((x, y) => x * y, 1);

export default Object.freeze(op);
