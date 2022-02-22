// === vector ===
// A simpler, functional approach to vector objects.
// vector is a factory function, not a constructor.

function vector(x, y) {
    return Object.freeze({
        get coordinates() {
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

// === example ===

// const v = vector(3, 4);
// v.coordinates; -> [3, 4]
// v.magnitude;   -> 5
// v.scale(2); 
// v.coordinates; -> [6, 8]
// v.magnitude;   -> 10
