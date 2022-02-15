// Numbered enums are auto-incremented collections of related but distinct values.

enum DirectionTS {
  Up,
  Down,
  Left,
  Right,
}

// === Compiled, numbered enum ===

"use strict";
let DirectionJS;
(function (Direction) {
    Direction[(Direction["Up"] = 0)] = "Up";
    Direction[(Direction["Down"] = 1)] = "Down";
    Direction[(Direction["Left"] = 2)] = "Left";
    Direction[(Direction["Right"] = 3)] = "Right";
})(DirectionJS || (DirectionJS = {}));

// === Enum value ===

// Direction = {
//     0: "Up",
//     1: "Down",
//     2: "Left",
//     3: "Right",
//     Up: 0,
//     Down: 1,
//     Left: 2,
//     Right: 3,
// };
