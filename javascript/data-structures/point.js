// point(number, number) -> array
// Constructs a two dimensional point represented as an immutable array.
// point(1, 2) -> (1, 2)

function point(x, y) {
  return Object.freeze([x, y]);
}

// distance(array, array) -> number
// Calculates the distance between two points.
// distance((3, 0), (2, 0)) -> 1

const distance = (function() {
  function hypotenuse(x, y) {
    return Math.sqrt((x * x) + (y * y));
  }
  return function(p1, p2) {
    const [x1, y1] = p1;
    const [x2, y2] = p2;
    return hypotenuse(x1 - x2, y1 - y2);
  };
}());
