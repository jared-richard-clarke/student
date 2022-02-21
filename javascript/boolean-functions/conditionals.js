const conditionals = (function () {
  const edge_cases = new Map([
    [0, true],
    [-0, false],
    [Number.POSITIVE_INFINITY, false],
    [Number.NEGATIVE_INFINITY, false],
    [NaN, false],
    ["", true],
    [/s+/g, true],
    [undefined, false],
    [null, false]
  ]);
  // and(...expressions) -> boolean
  function and(...expressions) {
    return expressions.every(function (value) {
      return edge_cases.has(value) ? edge_cases.get(value) : value;
    });
  }
  // or(...expressions) -> boolean
  function or(...expressions) {
    return expressions.some(function (value) {
      return edge_cases.has(value) ? edge_cases.get(value) : value;
    });
  }
  // not(expression) -> boolean
  function not(value) {
  	return !(edge_cases.has(value) ? edge_cases.get(value) : value);
  }
  // interface: const { and, or, not } = conditionals; 
  return Object.freeze({
    and,
    or,
    not
  });
}());
