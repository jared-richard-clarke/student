# Greatest Common Divisor

Euclid's algorithm for finding the greatest common divisor of two numbers
is a common example used in code tutorials. It is therefore an excellent 
tool for comparing the syntax of different programming languages.

## Factor

```factor
: gcd ( a b -- c )
    [ abs ] [
        [ nip ] [ mod ] 2bi gcd
    ] if-zero ;
```

## Go

```go
func gcd(x, y int) int {
    for y != 0 {
        x, y = y, x%y
    }
    return x
}
```

## OCaml

```ocaml
let rec gcd x y =
  if b = 0 then x else gcd y (x mod y);;
  
(* or *)

let rec gcd x y =
  match y with
    0 -> x
  | _ -> gcd y (x mod y)
```

## Scheme

```scheme
(define (GCD x y)
  (if (= y 0)
      x
      (GCD y (remainder x y))))
```
