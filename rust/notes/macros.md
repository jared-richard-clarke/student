# Rust Macros

**Source**: [The Rust Programming Language](https://doc.rust-lang.org/book/ch19-06-macros.html)

## Declarative Macro: vec!

A simplified version of the `vec!` macro as defined in the standard library.
Rust macros, unlike functions, can accept a variable number of arguments.

> The structure in the `vec!` body is similar to the structure of a `match` expression. 
> Here we have one arm with the pattern `( $( $x:expr ),* )`, followed by `=>` and the 
> block of code associated with this pattern. If the pattern matches, the associated 
> block of code will be emitted.
>
> — The Rust Programming Language

```rust
#[macro_export]
macro_rules! vec {
    ( $( $x:expr ),* ) => {
        {
            let mut temp_vec = Vec::new();
            $(
                temp_vec.push($x);
            )*
            temp_vec
        }
    };
}
```

## Macro Expansion

```rust
let v: Vec<u32> = vec![1, 2, 3];

// - expands ->

let v: Vec<u32> = {
    let mut temp_vec = Vec::new();
    temp_vec.push(1);
    temp_vec.push(2);
    temp_vec.push(3);
    temp_vec
}
```
