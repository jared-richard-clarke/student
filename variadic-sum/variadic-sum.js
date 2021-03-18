// JavaScript
function add(...numbers) {
    return numbers.reduce((sum, number) => sum + number);
}
add(2, 3, 5, 10);
