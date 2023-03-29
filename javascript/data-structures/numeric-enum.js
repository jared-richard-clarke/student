// enumerate(...values) -> Object { string | string(number): value | number }
// Takes a variable number of arguments and returns an immutable, enumerated object.
// Values are associated with a number from a series. The series starts at 1 and is incremented by 1.
// enumerate("green", "yellow", "red") ->
// {
//   1: "green",
//   2: "yellow",
//   3: "red",
//   green: 1,
//   yellow: 2,
//   red: 3,
// }

function enumerate(...values) {
    let number = 1;
    return Object.freeze(
        values.reduce(function (accum, value) {
            accum[value] = number;
            accum[number] = value;
            number += 1;
            return accum;
        }, Object.create(null))
    );
}
