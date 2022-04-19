# Closures in Rust

To capture variables from their environment, closures are encoded by three `Fn` traits:

1. `FnOnce` takes ownership of captured variables. A closure cannot take ownership of the same variable more than once.
2. `FnMut` can change the environment because it mutably borrows values.
3. `Fn` borrows values from the environment immutably.

## Caveats

Rust's memory model disallows regular functions and methods from capturing their environment.
