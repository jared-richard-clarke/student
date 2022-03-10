function vector(x, y) {
    return Object.freeze({
        get point() {
            return [x, y];
        },
        get magnitude() {
            return Math.sqrt(x * x + y * y);
        },
        scale(factor) {
            x *= factor;
            y *= factor;
        },
    });
}
