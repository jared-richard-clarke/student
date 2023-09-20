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

## Haskell

```haskell
gcd :: Int -> Int -> Int
gcd x y = gcd' (abs x) (abs y)
          where gcd' x 0 = x
                gcd' x y = gcd' y (rem x y)
```

## Joy

```joy
gcd  ==  [0 >] [dup [rem] dip swap] while pop
```

## Lua

```lua
-- tail recursive
function gcd(x, y)
    if y == 0 then
        return x
    end
    return gcd(y, x % y)
end

-- iterative
function gcd(x, y)
    while y != 0 do
        x, y = y, x % y
    end
    return x
end
```

## OCaml

```ocaml
let rec gcd x y =
  if y = 0 then x else gcd y (x mod y);;
  
(* or *)

let rec gcd x y =
  match y with
    0 -> x
  | _ -> gcd y (x mod y)
```

## Scheme

```scheme
(define (gcd x y)
  (if (= y 0)
      x
      (gcd y (remainder x y))))
```
