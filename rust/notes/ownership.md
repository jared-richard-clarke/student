# Rust Ownership and Borrowing

**Source**: [Rust Book](https://rust-book.cs.brown.edu/ch04-02-references-and-borrowing.html)

## Ownership Rules
- Each value in Rust has a variable that's called its *owner*.
- There can only be one owner at a time.
- When the owner goes out of scope, the value will be dropped.

## Borrow Checker

The borrow checker looks for potentially unsafe operations involving references.
The core idea behind the borrow checker is that variables have three kinds of permissions on their data.

- **Read( R )**: data can be copied to another location.
- **Write( W )**: data can be mutated in place.
- **Own( O )**: data can be owned or dropped.

By default a variable has **RO** permissions. **RWO** if prefaced by `let mut`.
References can temporarily remove these permissions to prevent undefined behavior.

### Paths

Permissions are defined on paths and not just variables. A path is anything you 
can put on the left-hand side of an assignment. Paths include:

- Variables, like `a`.
- Dereferences of paths, like `*a`.
- Array accesses of paths, like `a[0]`.
- Fields of paths, like `a.0` for tuples or `a.field` for structs.
- Any combination of the above, like `*((*a)[0].1)`.

### Reference Types

- **immutable references ( shared references )**: permit aliasing but disallow mutation.
- **mutable references ( unique references )**: temporarily provide mutable access to data without moving it — one alias at a time.

## Pointer Safety Principle

Data must outlive any references to it.
