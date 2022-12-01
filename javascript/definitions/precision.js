// precision() -> number
// Calculates the default precision of a binary floating point implementation.
// precision() -> 53 [env: Firefox]

function precision() {
    let n = 0;
    let x = 1;
    while (1 !== 1 + x) {
        x /= 2;
        n += 1;
    }
    return n;
}
