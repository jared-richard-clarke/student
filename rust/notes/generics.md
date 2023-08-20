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

impl Point<f32> {
//          ^ Concrete type. Only `Point<f32>` implements this method.
    fn distance_from_origin(&self) -> f32 {
        (self.x.powi(2) + self.y.powi(2)).sqrt()
    }
}
```
