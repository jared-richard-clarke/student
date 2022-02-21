// === Linked List ===
const node = function (data, node = null) {
    return {
        data,
        node,
    };
};

// === List Construction ===
const list = node(1, node(2, node(3)));
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
