// JavaScript
function map_list(action, list) {
    const result = [];
    for (let item of list) {
        result.push(action(item))
    }
    return result;
}
const result = map_list((number) => number ** 2, [1, 2, 3]);
// returns [1, 4, 9]
