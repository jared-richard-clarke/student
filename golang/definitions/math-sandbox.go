package mathsandbox

import "math"

// === Newton's Method for approximating square roots ===
// √x = the y such that y ≥ 0 and y ^ 2 = x
// Average y with x ÷ y in successive approximations — each average
// closer than the last.
// guess        quotient                 average
// 1            (2 ÷ 1) = 2              ((2 + 1) ÷ 2) = 1.5
// 1.5          (2 ÷ 1.5) = 1.3333       ((1.3333 ÷ 1.5) ÷ 2) = 1.4167
// 1.4167 ...

func SquareRoot(n float64) float64 {
	tolerance := 0.00001
	guess := 1.0
	approximate := func(g float64) float64 {
		return (g + n/g) / 2
	}
	next := approximate(guess)
	for {
		if math.Abs(guess-next) < tolerance {
			return next
		}
		guess = next
		next = approximate(guess)
	}
}

// === Greatest Common Divisor ===
// The greatest common divisor of x and y is the largest integer that divides both x and y with no remainder.
// GCD(10 5) -> 5

func GCD(x, y int) int {
	var prev int
	for {
		if y == 0 {
			return x
		}
		prev = x
		x = y
		y = prev % y
	}
}

// === Fibonacci ===
// A series of numbers in which each number is the sum of the two preceding numbers.
// fibonacci(4) -> 3 as in 1, 1, 2, 3

func Fibonacci(n int) int {
	x := 1
	y := 0
	prev := 0
	for count := n; count > 0; count -= 1 {
		prev = x
		x = x + y
		y = prev
	}
	return y
}

// === Factorial ===
// Where n! is the product of all positive integers less than or equal to n.
// factorial(4) -> 24

func Factorial(n int) int {
	result := 1
	count := n
	for count >= 1 {
		result *= count
		count -= 1
	}
	return result
}
