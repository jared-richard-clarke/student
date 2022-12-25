# Factorial

The factorial function is a common example used in many programming tutorials. 
It is therefore an excellent tool for comparing the syntax of different programming languages.

- A majority of these implementations (my programs) return the multiplicative identity for any number 
  less than or equal to 1 instead of raising an error.
- Many of these definitions are inefficient — consuming too much time and space.
  They are defined here solely to demonstrate the syntax of a particular language.

> x! is the product of all positive integers less than or equal to x.

## C

```c
// iterative
int factorial(int x) {
    if (x <= 1) {
        return 1;
    } else {
        int i, p = 1;
        for (i = 1; i <= x; i += 1) {
            p *= i;
        }
        return p;
    }
}

// recursive
int factorial(int x) {
    if (x <= 1) {
        return 1;
    } else {
        return x * factorial(x - 1);
    }
}
```

## Factor

```factor
! tail recursive
: tail-factorial ( accumulator n -- n! )
    dup 0 =
    [ drop ]
    [ [ * ] [ 1 - ] bi tail-factorial ]
    if ;

! word composition
USE: math.ranges
: factorial ( n -- n! )
    1 [a, b] product ;
```

## Go

```go
// iterative
func factorial(x int) int {
	if x <= 0 {
		return 1
	}
	p := 1
	for i := 1; i <= x; i += 1 {
		p *= i
	}
	return p
}

// recursive
func factorial(x int) int {
	if x <= 1 {
		return 1
	} else {
		return x * factorial(x-1)
	}
}
```

## Haskell

```haskell
-- recursive
factorial :: int -> int 
factorial 0 = 1  
factorial x = x * factorial (x - 1)

-- composition
factorial x = product [1..x]
```

## Julia 

- As implemented in Julia's Base library.

```julia
function factorial(n::Integer)
    n < 0 && throw(DomainError(n, "`n` must be nonnegative."))
    f::typeof(n*n) = 1
    for i::typeof(n*n) = 2:n
        f *= i
    end
    return f
end
```

## Lua

```lua
-- recursive
function factorial(x)
  return n > 0 and x * fact(x - 1) or 1
end

-- tail recursive
function factorial(x)
  product = 1
  if n == 0 then
    return product
  end
  return fact(x - 1, x * product)
end
```

## OCaml

```ocaml
(* pipe combinator: infix definition *)
let (|>) x f = f x

(* partial function application through currying *)    
let product = List.fold_left (fun x y -> x * y) 1             
let range x = List.init x (fun x -> x + 1)

(* function composition *)    
let factorial x = range x |> product

(* recursive *)
let rec factorial x =
  match x with
  | 1 -> 1
  | _ -> x * factorial (x - 1)
  
(* tail recursive *)
let factorial x =
  let rec loop x product =
    if x <= 1
    then product
    else loop (x - 1) (product * x)
  in
  loop x 1
```

## Python

```python
# iterative
def factorial(x):
    if x <= 1:
        return 1
    product = 1
    for i in range(1, x + 1):
        product *= i
    return product
    
# recursive
def factorial(n):
    return x * factorial(x - 1) if x > 1 else 1
```

## Rust

```rust
// recursive
fn factorial(x: i64) -> i64 {
    match x {
        x if x <= 1 => 1,
        _ => x * factorial(x - 1),
    }
}
// iterative
fn factorial(x: i64) -> i64 {
    (1..=x).product()
}
```

## Scheme

```scheme
;; recursive
(define (factorial x)
  (if (<= x 1)
      1
      (* x (factorial (- x 1)))))

;; tail-recursive
(define (factorial x)
  (let loop ([product 1]
             [number x])
    (if (<= number 1)
        product
        (loop (* product number) 
              (- number 1)))))
              
;; do loop
(define (factorial n)
  (do ([number n (- number 1)]
       [product 1 (* product number)])
    ((< number 1) product)))
    
;; function composition
(define (product xs)
  (fold-left * 1 xs))
    
(define (range x)
  (let ([x (+ x 1)])
    (cdr (iota x))))
      
(define (compose . functions)
  (lambda (arg)
    (fold-left (lambda (value function)
                 (function value))
	       arg
	       functions)))

(define factorial (compose range product))
```
