use std::fmt;
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

impl<T> fmt::Display for List<T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Cons(head, tail) => write!(f, "({} {})", head, tail),
            Nil => write!(f, "Nil"),
        }
    }
}
