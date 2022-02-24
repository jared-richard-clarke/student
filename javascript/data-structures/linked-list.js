// === Linked List ===
function link(first, rest = null) {
    const list = Object.create(null);
    list["first"] = first;
    list["rest"] = rest;
    return list;
}

// === Link Construction ===
const linked_list = link(1, link(2, link(3)));
// === value ===
// {
//     first: 1,
//     rest: {
//         first: 2,
//         rest: {
//             first: 3,
//             rest: null,
//         },
//     },
// };
