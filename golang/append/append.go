// Append function defined by Rob Pike in "Arrays, slices (and strings): The mechanics of 'append'"
package main

import "fmt"

func Append(slice []int, elements ...int) []int {
	n := len(slice)
	total := len(slice) + len(elements)
	if total > cap(slice) {
		// Reallocate. Grow to 1.5 times the new size, so we can still grow.
		newSize := total*3/2 + 1
		newSlice := make([]int, total, newSize)
		copy(newSlice, slice)
		slice = newSlice
	}
	slice = slice[:total]
	copy(slice[n:], elements)
	return slice
}
func main() {
	data := []int{1, 2, 3}
	data = Append(data, 4, 5, 6, 7)
	fmt.Println(data) // -> [1 2 3 4 5 6 7]
}
