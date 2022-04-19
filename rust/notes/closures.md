# Closures in Rust

To capture variables from their environment, closures are encoded by three `Fn` traits:

1. `FnOnce` takes ownership of captured variables. A closure cannot take ownership of the same variable more than once.
2. `FnMut` mutably borrows values.
3. `Fn` immutably borrows values.

## Caveats

Rust's memory model disallows regular functions and methods from capturing their environment.
