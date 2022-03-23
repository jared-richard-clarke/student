// === Vector ===
// Three patterns for creating Vector objects:
// 1. ES6 class syntax.
// 2. ES5 prototype syntax.
// 3. Factory function.

// === ES6 ===
class Vector_ES6 {
    constructor(x, y) {
        this.x = x;
        this.y = y;
    }
    static isVector(value) {
        return value instanceof Vector_ES6;
    }
    get tip() {
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
    Vector_ES5.isVector = function (value) {
        return value instanceof Vector_ES5;
    };
    Object.defineProperty(Vector_ES5.prototype, "tip", {
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

// === factory function ===
// Simplest and most secure pattern for creating vector objects.
function vector(x, y) {
    return Object.freeze({
        get tip() {
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
