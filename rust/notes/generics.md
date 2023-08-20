# Rust Generics

## Method Definitions

```rust
struct Point<T> {
    x: T,
    y: T,
}

impl<T> Point<T> {
//   ^ By declaring `T` as a generic type after `impl`, Rust can identify
//     that the type in the angle brackets after `Point` is a generic type
//     rather than a concrete type.
    fn x(&self) -> &T {
        &self.x
    }
}
```
