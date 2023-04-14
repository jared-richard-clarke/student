# JavaScript Callbacks

```javascript
// sequence
function one(value) {
    return value + 1;
}

function two(value) {
    return value + 1;
}

function three(value) {
    return value + 1;
}

function sequence() {
    let result = 0;
    result = one(result);
    result = two(result);
    result = three(result);
    return result;
}

// callback
function call_one(value, callback) {
    const result = value + 1;
    callback(result);
}

function call_two(value, callback) {
    const result = value + 1;
    callback(result);
}

function call_three(value, callback) {
    const result = value + 1;
    callback(result);
}

// callback hell
function call_sequence() {
    call_one(0, (one) => {
        call_two(one, (two) => {
            call_three(two, (three) => {
                return three;
            });
        });
    });
}
```
