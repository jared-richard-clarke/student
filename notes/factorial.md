# Factorial

The factorial function is a common example used in many programming tutorials. 
It is therefore an excellent tool for comparing the syntax and semantics of different programming languages.
A majority of these implementations (my programs) return the multiplicative identity for any number 
less than or equal to 0 instead of raising an error.

> n! is the product of all positive integers less than or equal to n.

## OCaml

```ocaml
(* pipe combinator: infix definition *)
let (|>) x f = f x

(* partial function application through currying *)    
let product = List.fold_left (fun x y -> x * y) 1

(* tail-recursive loop *)              
let range start stop =
  let rec loop count total =
    if count < start then total else loop (count - 1) (count :: total)
  in loop stop []

(* putting it all together *)    
let factorial n = (range 1 n) |> product
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
  (let loop ([result 1]
             [number n])
    (if (< number 1)
        result
        (loop (* result number) 
              (- number 1)))))
              
;; do loop
(define (factorial n)
  (do ([number n (- number 1)]
       [result 1 (* result number)])
    ((< number 1) result)))
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

## Forth

- Forth programmers use a special stack notation to describe their programs.
- The basic form is `( before -- after )`
- The dashes separate what is on the stack before and after a `word` executes.

```forth
 : factorial  ( n -- n! )  recursive
    dup 1 >  if   dup 1-  factorial  *  then
 ;
 
 ( or )
 
 : factorial  ( n -- n! )
    dup 1 >  if   dup 1-  recurse  *  then
 ;
```
