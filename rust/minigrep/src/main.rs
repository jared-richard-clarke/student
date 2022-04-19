use std::env;
use std::process;

use minigrep::Config;

// I/O Project: Building a Command Line Program
// Pulled from [The Rust Programming Language](https://doc.rust-lang.org/book/ch12-00-an-io-project.html), Chapter 12

fn main() {
    // std::env::args will panic if any argument contains invalid unicode.
    let config = Config::new(env::args()).unwrap_or_else(|err| {
        eprintln!("Problem parsing arguments: {}", err);
        process::exit(1);
    });

    if let Err(e) = minigrep::run(config) {
        eprintln!("Application error: {}", e);
        process::exit(1);
    };
}
