# Bitwise Addition and Subtraction

Original code by Douglas Crockford, **Add and Subtract**

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
