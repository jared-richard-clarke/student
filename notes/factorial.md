# Factorial

The factorial function is a common example used in many programming tutorials. 
It is therefore an excellent tool for comparing the syntax and semantics of different programming languages.
A majority of these implementations (my programs) return the multiplicative identity for any number 
less than or equal to 0 instead of raising an error.

> n! is the product of all positive integers less than or equal to n.

## C

```c
// loop
int factorial(int x) {
    if (x <= 0) {
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
    if (x <= 0) {
        return 1;
    } else {
        return x * factorial(x - 1);
    }
}
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

## Scheme

```scheme
;; recursive
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

;; tail-recursive
(define (factorial n)
  (let loop ([product 1]
             [number n])
    (if (< number 1)
        product
        (loop (* product number) 
              (- number 1)))))
              
;; do loop
(define (factorial n)
  (do ([number n (- number 1)]
       [product 1 (* product number)])
    ((< number 1) product)))
```

## Go

```go
func factorial(n int) int {
	if n < 0 {
		return 1
	}
	product := 1
	for i := 1; i <= n; i += 1 {
		product *= i
	}
	return product
}
```

## Python

```python
def factorial(n):
    if n < 0:
        return 1
    product = 1
    for i in range(1, n + 1):
        product *= i
    return product
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

## Factor

```factor
! tail recursive
: tail-factorial ( accumulator n -- n! )
    dup 0 =
    [ drop ]
    [ [ * ] [ 1 - ] bi tail-factorial ]
    if ;

! using libraries
USE: math.ranges
: factorial ( n -- n! )
    1 [a, b] product ;
```
