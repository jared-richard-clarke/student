# Rust Iterators

## Iterator Core

```rust
trait Iterator {
    type Item;
    fn next(&mut self) -> Option<Self::Item>;
}
```

An iterator has a method, `next`, which when called, returns an `Option<Item>`. 
Calling `next` will return `Some(Item)` as long as there are elements, and once 
they’ve all been exhausted, will return `None` to indicate that iteration is finished.

## Three Forms of Iteration

1. `iter()`, which iterates over `&T`. ( immutable reference )
2. `iter_mut()`, which iterates over `&mut T`. ( mutable reference )
3. `into_iter()`, which iterates over `T`. ( ownership )

## Implementing an Iterator

Creating an iterator involves two steps: creating a `struct` to hold the iterator’s state, 
and then implementing `Iterator` for that `struct`.

```rust
use std::iter::Iterator

struct Counter {
    count: usize,
}

impl Counter {
    fn new() -> Counter {
        Counter { count: 0 }
    }
}

impl Iterator for Counter {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        if self.count < 5 {
            self.count += 1;
            Some(self.count)
        } else {
            None
        }
    }
}
```

## `for` loops and `IntoIterator`

Rust's `for` loop is syntactic sugar for iterators.

```rust
let values = vec![1, 2, 3, 4, 5, 6, 7];

for x in values {
    println!("{x}");
}

// - compiles into ->

let values = vec![1, 2, 3, 4, 5, 6, 7];
{
    let result = match IntoIterator::into_iter(values) {
        mut iter => loop {
            let next;
            match iter.next() {
                Some(val) => next = val,
                None => break,
            };
            let x = next;
            let () = { println!("{x}"); };
        },
    };
    result
}
```

## Adapters

Functions which take an `Iterator` and return another `Iterator`, such as `map`, `take`, and `filter`.

## Infinity

Because iterators are lazily evaluated, they can be infinite in range.

```rust
let numbers = 0..;

for number in numbers.take(7) {
    println!("{number}");
}
```
