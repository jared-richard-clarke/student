// hypotenuse(...numbers) -> number
// Returns the square root of the sum of squares of its arguments.
// hypotenuse(3, 4) -> 5

function hypotenuse(...numbers) {
    return Math.hypot(...numbers);
}

// point(number, number) -> array
// Constructs a two-dimensional point represented as an immutable array.
// point(1, 2) -> [1, 2]

function point(x, y) {
    return Object.freeze([x, y]);
}

// ORIGIN: point of origin in a two-dimensional coordinate system.

const ORIGIN = point(0, 0);

// segment(array, array) -> number
// Calculates the distance between two points.
// segment(point(3, 0), point(2, 0)) -> 1

function segment(p1, p2) {
    const [x1, y1] = p1;
    const [x2, y2] = p2;
    return hypotenuse(x1 - x2, y1 - y2);
}

// path(...point) -> array
// Constructs an array of two-dimensional points.
// path(point(1, 2), point(3, 4)) -> [[1, 2], [3, 4]]

function path(...points) {
    return Object.freeze(points);
}
