// === Linked List Prototype ===
// Use prototypal inheritance. Every link in the list need not have its own methods.
const proto_link = {
    traverse: function (action) {
        this.head = action(this.head);
        if (this.tail !== null) {
            this.tail.traverse(action);
        }
    },
};

// === Linked List ===
function link(head, tail = null) {
    const list = Object.create(proto_link);
    list["head"] = head;
    list["tail"] = tail;
    return list;
}

// === Linked List Construction ===
const list = link(1, link(2, link(3)));
list.traverse((x) => x * x);
// === value ===
// {
//     head: 1,
//     tail: {
//         head: 4,
//         tail: {
//             head: 9,
//             tail: null,
//         },
//     },
// };
