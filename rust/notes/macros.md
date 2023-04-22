# Rust Macros

**Source**: [The Rust Programming Language](https://doc.rust-lang.org/book/ch19-06-macros.html)

## vec!

A simplified version of the `vec!` macro as defined in the standard library.
Rust macros, unlike functions, can accept a variable number of arguments.

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
