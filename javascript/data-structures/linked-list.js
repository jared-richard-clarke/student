// === Linked List ===
// Defined using LISP terminology.
const node = function (data, node = null) {
    return {
        data: data,
        node: node,
    };
};

const cons = function (node, list) {
    node.node = list;
    return node;
};
const car = function (list) {
    return list.data;
};
const cdr = function (list) {
    return list.node;
};
// === List Construction ===
const list = node(1, node(2, node(3)));
// === value ===
// {
//     data: 3,
//     node: {
//         data: 2,
//         node: {
//             data: 1,
//             node: null,
//         },
//     },
// };
