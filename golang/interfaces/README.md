# Interfaces
An interface type is a set of method signatures. It can store any concrete (non-interface) value 
as long as that value implements the interface's methods. 

## Interface Values
Conceptually, an interface is a tuple of a concrete type and a value.
```
(type, value)
```

## Type Assertion
Type assertion provides access to an interface's underlying value. It returns 
the underlying value and a boolean that reports whether the assertion succeeded.
```go
var number interface{} = 7
v, ok := number.(int) // v == 7, ok == true 
```

## Type Switch
A type switch is a switch statement that performs a series of type assertions.
```go
switch v := i.(type) {
case int64:
    return v + 7
case float64:
    return v + 7.0
default:
    return error.New("invalid type")
}
```

## Alias Any
The type `any` is an alias for any empty interface value. It is equal to `interface{}` in all ways.
```go
type any interface{}
```
