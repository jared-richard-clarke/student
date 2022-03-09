// === Linked List: Functional ===
function link(head, tail = null) {
    const list = Object.create(null);
    list["head"] = head;
    list["tail"] = tail;
    return list;
}
function traverse(list, action) {
    if (list.tail === null) {
        return link(action(list.head));
    }
    return link(action(list.head), traverse(list.tail, action));
}
// === Linked List Construction ===
const list = link(3, link(2, link(1)));
const square_list = traverse(list, (x) => x * x);

// === list ===
// {
//     head: 3,
//     tail: {
//         head: 2,
//         tail: {
//             head: 1,
//             tail: null,
//         },
//     },
// };

// === square_list ===
// {
//     head: 9,
//     tail: {
//         head: 4,
//         tail: {
//             head: 1,
//             tail: null,
//         },
//     },
// };

// === Linked List: Object Oriented ===
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
function prolink(head, tail = null) {
    const list = Object.create(proto_link);
    list["head"] = head;
    list["tail"] = tail;
    return list;
}

// === Linked List Construction ===
const prolist = prolink(3, prolink(2, prolink(1)));
prolist.traverse((x) => x * x);
// === prolist: post-traversal ===
// {
//     head: 9,
//     tail: {
//         head: 4,
//         tail: {
//             head: 1,
//             tail: null,
//         },
//     },
// };
