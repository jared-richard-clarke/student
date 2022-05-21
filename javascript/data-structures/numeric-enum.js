// enumerate(...values) -> Object { string(number): value | number }
// Takes a variable number of arguments and returns an immutable, enumerated object.
// Values are associated with a number from a series. The series starts at 0 and is incremented by 1.
// enumerate("green", "yellow", "red") ->
// {
//   0: "green",
//   1: "yellow",
//   2: "red",
//   green: 0,
//   yellow: 1,
//   red: 2,
// }

const enumerate = function (...values) {
    const result = Object.create(null);
    let number = 0;
    values.forEach(function (value) {
        result[value] = number;
        result[number] = value;
        number += 1;
    });
    return Object.freeze(result);
};
