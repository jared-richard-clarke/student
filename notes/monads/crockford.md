# Monads

[Monads and Gonads](https://www.youtube.com/watch?v=b0EF0VTs9Dc)
by Douglas Crockford

```text
// All three functions return a monad.

function unit(value)

function bind(monad, function (value))
```

Monads are, generally, an object.

## Axioms

### Functional Notation

```
- bind(unit(value), f) -> f(value)

- bind(monad, unit) -> monad

- bind(bind(monad, f), g)
  ->
  bind(monad, function (value) {
      return bind(f(value), g);
  })
```

### Methodical Notation

```
- unit(value).bind(f) -> f(value)

- monad.bind(unit) -> monad

- monad.bind(f).bind(g) 
  ->
  monad.bind(function (value) {
      return f(value).bind(g)
  })
```

## Macroid

```javascript

function MONAD() {
    return function unit(value) {
        var monad = Object.create(null);
        monad.bind = function (func) {
            return func(value);
        };
        return monad;
    };
}
```

## Identity Monad

```javascript
var identity = MONAD();
var monad = identity("Hello world.");
monad.bind(alert);
```

## Interstate (2001)

Interstate, like JQuery, wraps DOM nodes in monads for easy manipulation.

```javascript
new Interform("text")
    .moveTo(100, 100)
    .setSize(400, 32)
    .moveInside()
    .setBgColor("pink")
    .select()
    .setZIndex(20000)
    .on("escapekey", "erase");
```

## Monad Modified

```javascript
function MONAD(modifier) {
    var prototype = Object.create(null);
    function unit(value) {
        var monad = Object.create(prototype);
        monad.bind = function (func, args) {
            return func(value, ...args);
        };
        if (typeof modifier === "function") {
            modifier(monad, value);
        }
        return monad;
    }
    return unit;
}
```

## Maybe Monad

Solution for `null` values.

`NaN` was invented to prevent programs from crashing after dividing a number by zero.
Checking for `NaN` at the end of an operation is much nicer than putting a guard
around each operator to prevent a divide-by-zero error.

```javascript
var maybe = MONAD(function (monad, value) {
    if (value === null || value === undefined) {
        monad.is_null = true;
        monad.bind = function () {
            return monad;
        };
    }
});

var monad = maybe(null);
monad.bind(alert); // -> nothing happens, no crashing
```
