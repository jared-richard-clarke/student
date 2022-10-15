# Error Propogation in Rust

[Recoverable Errors](https://doc.rust-lang.org/book/ch09-02-recoverable-errors-with-result.html)

## Using Match Expressions

```rust
#![allow(unused)]
fn main() {
    use std::fs::File;
    use std::io::{self, Read};
    fn read_username_from_file() -> Result<String, io::Error> {
        let username_file_result = File::open("hello.txt");
        let mut username_file = match username_file_result {
            Ok(file) => file,
            Err(e) => return Err(e),
        };
        let mut username = String::new();
        match username_file.read_to_string(&mut username) {
            Ok(_) => Ok(username),
            Err(e) => Err(e),
        }
    }
}
```

## Using The `?` Operator

```rust
#![allow(unused)]
fn main() {
    use std::fs::File;
    use std::io;
    use std::io::Read;
    fn read_username_from_file() -> Result<String, io::Error> {
        let mut username = String::new();
        File::open("hello.txt")?.read_to_string(&mut username)?;
        Ok(username)
    }
}
```
