# Error Propagation in Rust

[Recoverable Errors](https://doc.rust-lang.org/book/ch09-02-recoverable-errors-with-result.html)

## Using Match Expressions

```rust
use std::fs::File;
use std::io::{self, Read};

fn main() {
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
use std::fs::File;
use std::io;
use std::io::Read;

fn main() {
    fn read_username_from_file() -> Result<String, io::Error> {
        let mut username = String::new();

        File::open("hello.txt")?.read_to_string(&mut username)?;

        Ok(username)
    }
}
```

The `?` operator can only be used in functions that return `Result` or `Option`.
In order to propagate errors in the main executable, `main` must return the `Result` type.
The `main` function may return any types that implement the `std::process::Termination` trait.

```rust
use std::error::Error;
use std::fs::File;

fn main() -> Result<(), Box<dyn Error>> {
    let greeting_file = File::open("hello.txt")?;

    Ok(())
}
```

## Using `fs::read_to_string`

```rust
use std::fs;
use std::io;

fn main() {
    fn read_username_from_file() -> Result<String, io::Error> {
        fs::read_to_string("hello.txt")
    }
}
```
