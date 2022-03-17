# Interfaces
I examine Go interfaces through my own code and examples pulled from [go.dev](https://go.dev/).

Go is a statically typed language, meaning every variable has a type that is fixed and known at compile time. 
A value with type `int` cannot be assigned to a variable with type `float64` without explicit conversion.

An interface variable, however, can store any concrete (non-interface) value as long as that value implements the interface's methods.

## Interface Values
An interface has two components: a concrete type and value. I based the diagrams below on figures 7.1 through 7.3 in [The Go Programming Language](https://www.gopl.io/) by Alan A. A. Donovan and Brian W. Kernighan.

|       | `var writer io.Writer` | `writer = os.Stdout` | `writer = new(bytes.Buffer)` |
|-------| ---------------------- | -------------------- | ---------------------------- |
| type  | `nil`                  | `*os.Stdout`         | `*bytes.Buffer`              |
| value | `nil`                  |  -> fd int 1(stdout) |  -> data \[]byte             |
