// Linked List pulled from Rust By Example
// https://doc.rust-lang.org/rust-by-example/custom_types/enum/testcase_linked_list.html?highlight=link#testcase-linked-list

use crate::List::*;

enum List {
    Cons(u32, Box<List>),
    Nil,
}

impl List {
    fn new() -> List {
        Nil
    }

    fn prepend(self, elem: u32) -> List {
        Cons(elem, Box::new(self))
    }

    fn length(&self) -> u32 {
        match *self {
            // "ref" annotates pattern bindings to make them borrow rather than move.
            Cons(_, ref tail) => 1 + tail.length(),
            Nil => 0
        }
    }

    fn stringify(&self) -> String {
        match *self {
            Cons(head, ref tail) => {
                format!("{}, {}", head, tail.stringify())
            },
            Nil => {
                format!("Nil")
            },
        }
    }
}
