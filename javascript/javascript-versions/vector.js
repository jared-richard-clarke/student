// new Vector(number, number) -> Vector{x, y}
// properties { coordinates -> array, magnitude -> number, scale -> void }

// === ES6 ===
class Vector_ES6 {
    constructor(x, y) {
        this.x = x;
        this.y = y;
    }
    get coordinates() {
        return [this.x, this.y];
    }
    magnitude() {
        return Math.sqrt(x * x + y * y);
    }
    scale(factor) {
        this.x *= factor;
        this.y *= factor;
    }
}

// === ES5 ===
("use strict");
var Vector_ES5 = (function () {
    function Vector_ES5(x, y) {
        this.x = x;
        this.y = y;
    }
    Object.defineProperty(Vector_ES5.prototype, "coordinates", {
        get: function () {
            return [this.x, this.y];
        },
        enumerable: false,
        configurable: true,
    });
    Vector_ES5.prototype.magnitude = function () {
        return Math.sqrt(x * x + y * y);
    };
    Vector_ES5.prototype.scale = function (factor) {
        this.x *= factor;
        this.y *= factor;
    };
    return Vector_ES5;
})();
