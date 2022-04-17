// point(number, number) -> array
// Constructs a two-dimensional point represented as an immutable array.
// point(1, 2) -> [1, 2]

function point(x, y) {
  return Object.freeze([x, y]);
}

// path(...point) -> array
// Constructs an array of two-dimensional points.
// path(point(1, 2), path(3, 4)) -> [[1, 2], [3, 4]]

function path(...points) {
  return Object.freeze(points);
}

// distance(array, array) -> number
// Calculates the distance between two points.
// distance(point(3, 0), point(2, 0)) -> 1

function distance(p1, p2) {
    const [x1, y1] = p1;
    const [x2, y2] = p2;
    return hypotenuse(x1 - x2, y1 - y2);
}

// hypotenuse(number, number) -> number
// Computes the longest side of a right triangle. (Helper Function)
// hypotenuse(3, 4) -> 5

function hypotenuse(x, y) {
  return Math.sqrt((x * x) + (y * y));
