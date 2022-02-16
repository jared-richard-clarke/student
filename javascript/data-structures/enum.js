// === Enumeration: numeric ===
// A series of related but distinct values associated 
// with a number that is auto-incremented from 0.

const enumerate = function (...values) {
    const result = {};
    let number = 0;
    values.forEach(function (value) {
        result[(result[value] = number)] = value;
        number += 1;
    });
    return result;
};

const traffic_light = enumerate("green", "yellow", "red");
// === value ====
// {
//   0: "green",
//   1: "yellow",
//   2: "red",
//   green: 0,
//   yellow: 1,
//   red: 2,
// }
