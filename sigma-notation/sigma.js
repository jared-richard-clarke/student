// JavaScript
function sigma(operation, start, stop) {
    if (start > stop) {
        return 0;
    }
    let sum = 0;
    let value = start;
    for (value; value <= stop; value += 1) {
        sum += operation(value);
    }
    return sum;
}

// result = 30
const result = sigma((x) => x * x, 1, 4)
