// === Linked List ===
function link(data, node = null) {
    const list = Object.create(null);
    list["data"] = data;
    list["node"] = node;
    return list;
}

// === Link Construction ===
const linked_list = link(1, link(2, link(3)));
// === value ===
// {
//     data: 1,
//     node: {
//         data: 2,
//         node: {
//             data: 3,
//             node: null,
//         },
//     },
// };
