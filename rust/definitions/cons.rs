// Example pulled from The Rust Programming Language, chapter 15.1

enum List {
    Cons(i32, Box<List>),
    Nil,
}

use crate::List::{Cons, Nil};

// Potentially infinite in size, recursive data structures cannot exist on the stack.
// They must be allocated on the heap and pointed to by reference.
// Box<T> provides this facility.

fn main() {
    // The Box<T> type is a smart pointer. It implements the Deref trait,
    // which allows Box<T> values to be treated like references.
    // Box<T> also implements the Drop trait, which cleans up heap data
    // when the Box<T> pointer falls out of scope.

    let list = Cons(1, Box::new(Cons(2, Box::new(Cons(3, Box::new(Nil))))));
}
