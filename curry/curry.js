// JavaScript
function add(x) {
    return function (y) {
        return x + y;
    }
}

const add_1 = add(1);
const seven = add_1(6);

const eleven = add(1)(10);
