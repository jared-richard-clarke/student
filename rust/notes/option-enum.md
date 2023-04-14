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

## `unwrap`

Returns the contained `Some` value, consuming the `self` value.
Panics otherwise.

```rust
impl<T> Option<T> {
  fn unwrap(self) -> T {
    match self {
      Some(t) => t,
      None => panic("called `Option::unwrap()` on a `None` value"),
    }
  }
}
```

## `unwrap_or`

Returns the contained `Some` value or a provided default.

```rust
impl<T> Option<T> {
  fn unwrap_or(self, default: T) -> T {
    match self {
      Some(t) => t,
      None => default
    }
  }
}
```
