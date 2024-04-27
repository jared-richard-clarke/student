// Allows variants of enum to exist without prefix.
use List::*;

enum List<T> {
    Cons(T, Box<List<T>>),
    Nil,
}

impl<T> List<T> {
    fn new() -> List<T> {
        Nil
    }

    fn prepend(self, element: T) -> List<T> {
        Cons(element, Box::new(self))
    }

    fn length(&self) -> u32 {
        match self {
            Cons(_, tail) => 1 + tail.length(),
            Nil => 0,
        }
    }
}

impl<T: std::fmt::Display> List<T> {
    fn stringify(&self) -> String {
        match self {
            Cons(head, tail) => {
                format!("{}, {}", head, tail.stringify())
            }
            Nil => "Nil".to_string(),
        }
    }
}
