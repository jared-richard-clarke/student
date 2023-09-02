# Factorial

The factorial function is a common example used in many programming tutorials. 
It is therefore an excellent tool for comparing the syntax of different programming languages.

- A majority of these implementations (my programs) return the multiplicative identity for any number 
  less than or equal to 1 instead of raising an error.
- Many of these definitions are inefficient — consuming too much time and space.
  They are defined here solely to demonstrate the syntax of a particular language.

## Definition

> x! is the product of all positive integers less than or equal to x.

## APL

```apl
⍝ built-in definition: !
!4 ⍝ -> 24

⍝ multiply a vector of integers from 1 through "n"
Factorial←{×/⍳⍵}

Factorial 4 ⍝ -> 24
```

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
: factorial ( accumulator n -- n! )
    dup 0 =
    [ drop ]
    [ [ * ] [ 1 - ] bi factorial ]
    if ;

! word composition
USE: math.ranges
: factorial ( n -- n! )
    dup 1 >
    [ [1..b] product ]
    [ drop 1 ]
    if ;
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
-- Using Integer over Int because Integer has larger range.
-- recursive
factorial :: Integer -> Integer
factorial x
    | x <= 0 = 1
    | otherwise = x * factorial (x - 1)

-- composition
-- Non-strict semantics allow Haskell to consume the range operation
-- without producing an intermediate list.
factorial :: Integer -> Integer
factorial x = product [1..x]
```

## io

```io
Number ! := method(self * (self - 1)!) 
0 ! := 1 

4! // -> 24
```

## Joy

```joy
# === recursive definition ===

factorial ==
    [0 =]
    [pop 1]
    [dup 1 - factorial *]
    ifte.

# === recursive combinator ===

factorial ==
    [null]
    [succ]
    [dup pred]
    [i *]
    genrec.
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

## OCaml

```ocaml
(* recursive *)
let rec factorial x =
  match x with
  | x when x <= 1 -> 1
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

## Prolog

```prolog
% reads as "the factorial of X is Y"
factorial(X, Y) :- 
      X > 0, 
      X2 is X - 1,
      factorial(X2, Y2), 
      Y is X * Y2.
```

## Ruby

```ruby
class Integer
    def factorial_recur
        self <= 1 ? 1 : self * (self - 1).factorial_recur
    end
    def factorial_iter
        f = 1 
        for i in 1..self
            f *= i 
        end 
        f
    end
    def factorial_reduce
        # Creates a range object, which implements lazy evaluation.
        self <= 1 ? 1 : (1..self).reduce(:*)
    end
    alias :factorial :factorial_iter
end

4.factorial # -> 24
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
// composition
fn factorial(x: i64) -> i64 {
    // Creates an iterator, which implements lazy evaluation.
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
(define (factorial x)
  (do ([number x  (- number 1)]
       [product 1 (* product number)])
    [(<= number 1) product]))
```

## Smalltalk

```smalltalk
"=== class ==="
Number extend [
  factorial [
    (self <= 1) 
        ifTrue: [1]
        ifFalse: [(1 to: self) fold: [:a :b | a * b]]
  ]
].

"-> 24"
4 factorial.

"=== block ==="
factorial := [:n | n <= 1
                 ifTrue: [1]
                 ifFalse: [n * (factorial value: n - 1)]].
		 
"-> 24"
factorial value: 4
```

## Web Assembly: `.wat` notation

```wasm
(module
  (func $factorial (param f64) (result f64)
    local.get 0
    f64.const 1
    f64.lt
    if (result f64)
      f64.const 1
    else
      local.get 0
      local.get 0
      f64.const 1
      f64.sub
      call $factorial
      f64.mul
    end)
  (export "factorial" (func $factorial)))
```
