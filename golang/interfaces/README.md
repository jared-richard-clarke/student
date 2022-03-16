# Interfaces
I examine Go interfaces through my own code and examples pulled from [go.dev](https://go.dev/).

Go is a statically typed language, meaning every variable has a type that is fixed and known at compile time. 
A value with type `int` cannot be assigned to a variable with type `float64` without explicit conversion.

An interface variable, however, can store any concrete (non-interface) value as long as that value implements the interface's methods.

## Interface Values
An interface has two components: a concrete type and value. I based the diagram below on figures 7.1 through 7.3 in [The Go Programming Language](https://www.gopl.io/) by Alan A. A. Donovan and Brian W. Kernighan.

```
// === writer 1 ===
var writer io.Writer

// === writer 2 ===
writer = os.Stdout

// === writer 3 ===
writer = new(bytes.Buffer)

```
<figure>
  <img src="https://user-images.githubusercontent.com/80301412/158706972-1eacf4f4-eaea-4f78-887e-f28ffe18d4ea.png" width="550" alt=""/>
  <figcaption>Three interface examples: a nil, *os.Stdout, and *bytes.Buffer interface value.</figcaption>
</figure>
