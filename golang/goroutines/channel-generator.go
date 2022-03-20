// "boring" generates a channel and a series of values to send to that channel.
// Example pulled from "https://www.youtube.com/watch?v=f6kdp27TYZs"

package main

import (
	"fmt"
	"math/rand"
	"time"
)

func boring(msg string) <-chan string { // Returns receive-only channel of strings.
	c := make(chan string)
	go func() {
		for i := 0; ; i += 1 {
			c <- fmt.Sprintf("%s %d", msg, i)
			time.Sleep(time.Duration(rand.Intn(1e3)) * time.Millisecond)
		}
	}()
	return c // Return the channel to the caller.
}
func main() {
	joe := boring("Joe")
	ann := boring("Ann")
	for i := 0; i < 5; i += 1 {
		fmt.Println(<-joe)
		fmt.Println(<-ann)
	}
	fmt.Println("You're both boring. I'm leaving.")
}

// === output ===
// Joe 0
// Ann 0
// Joe 1
// Ann 1
// Joe 2
// Ann 2
// Joe 3
// Ann 3
// Joe 4
// Ann 4
// You're both boring. I'm leaving.
