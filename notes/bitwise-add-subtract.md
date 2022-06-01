# Bitwise Addition and Subtraction

[add **and** subtract](https://www.crockford.com/add.html) 
by Douglas Crockford

---

```javascript

function add(augend, addend) {
    const sum = augend ^ addend;
    const carry = augend & addend;
    return (
        carry === 0
        ? sum
        : add(sum, carry << 1)
    );
}

function negate(integer) {
    return add(~integer, 1);
}

function subtract(minuend, subtrahend) {
    return add(minuend, negate(subtrahend));
}

```
