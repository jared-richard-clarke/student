// Factory functions that produce folding operations.

function fold_left(operation) {
    return Object.freeze(function (...operands) {
        return operands.reduce((accum, operand) => 
            operation(accum, operand)
        );
    });
}

function fold_right(operation) {
    return Object.freeze(function (...operands) {
        return operands.reduceRight((accum, operand) =>
            operation(operand, accum)
        );
    });
}
