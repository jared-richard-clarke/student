# Interfaces
I examine Go interfaces through my own code and examples pulled from [go.dev](https://go.dev/).

Go is a statically typed language, meaning every variable has a type that is fixed and known at compile time. 
A value with type `int` cannot be assigned to a variable with type `float64` without explicit conversion.

An interface variable, however, can store any concrete (non-interface) value as long as that value implements the interface's methods.
