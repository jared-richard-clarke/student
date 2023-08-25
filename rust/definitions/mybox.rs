// Example pulled from The Rust Programming Language, chapter 15.2
// Side Note: MyBox does not store data on the heap like Box<T>.
// This is an an example implementation of the Deref trait.

use std::ops::Deref;

impl<T> Deref for MyBox<T> {
    // associated type: an alternative syntax for generic types
    type Target = T;

    // Returns "MyBox" wrapped in a reference, a type the compiler can dereference.
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

fn hello(name: &str) {
    println!("Hello, {}!", name);
}

fn main() {
    let x = 7;
    let y = MyBox::new(x);

    assert_eq!(7, x);
    assert_eq!(7, *y);

    // === with deref coercion ===
    let m = MyBox::new(String::from("Rust"));
    hello(&m);

    // === without deref coercion ===
    // Without deref coercion, the programmer would have to manually unwrap and rewrap
    // reference types to satisfy the compiler.
    //
    // let m = MyBox::new(String::from("Rust"));
    // hello(&(*m)[..]);
}
