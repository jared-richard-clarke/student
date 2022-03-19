// Goroutine "fibonacci" sends values to channel "c". 
// "fibonacci", the sender, closes "c", the receiver, at the end of its loop.
// Example pulled from "https://go.dev/tour/concurrency/4"

package main

import (
	"fmt"
)

func fibonacci(n int, c chan int) {
	x, y := 0, 1
	for i := 0; i < n; i += 1 {
		c <- x
		x, y = y, x+y
	}
	close(c)
}

func main() {
	c := make(chan int, 10)
	go fibonacci(cap(c), c)
	for i := range c {
		fmt.Println(i)
	}
}
