// Example pulled from The Rust Programming Language, chapter 15.2
// Side Note: MyBox does not store data on the heap like Box<T>. 
// This is an an example implementation of the Deref trait.

use std::ops::Deref;

impl<T> Deref for MyBox<T> {
    // associated type: an alternative syntax for generic types
    type Target = T; 

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

struct MyBox<T>(T);

impl<T> MyBox<T> {
    fn new(x: T) -> MyBox<T> {
        MyBox(x)
    }
}

fn main() {
    let x = 7;
    let y = MyBox::new(x);

    assert_eq!(7, x);
    assert_eq!(7, *y);
}
