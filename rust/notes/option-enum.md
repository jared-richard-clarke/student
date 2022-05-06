# Rust: `Option<T>`

Bottom value `null` is useful but difficult to use safely. If a program uses `null` as if it were a not-null value, an error will be thrown.
Rust provides `Option<T>` — an enum that encodes a value that is either present or absent.

```rust
// as defined in the standard library
enum Option<T> {
    None,
    Some(T),
}
```
