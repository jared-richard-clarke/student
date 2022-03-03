// === Linked List ===
function link(head, tail = null) {
    const list = Object.create(null);
    list["head"] = head;
    list["tail"] = tail;
    return list;
}

function traverse(list, action) {
  list.head = action(list.head);
  if (list.tail !== null) {
    traverse(list.tail, action);
  }
}

// === Link Construction ===
const linked_list = link(1, link(2, link(3)));
// === value ===
// {
//     head: 1,
//     tail: {
//         head: 2,
//         tail: {
//             head: 3,
//             tail: null,
//         },
//     },
// };
