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
	c := boring("boring") // Function returning channel.
	for i := 0; i < 5; i += 1 {
		fmt.Printf("You say: %q/n", <-c)
	}
	fmt.Println("You're boring. I'm leaving.")
}

// You say: "boring" 0
// You say: "boring" 1
// You say: "boring" 2
// You say: "boring" 3
// You say: "boring" 4
// You're boring. I'm leaving.
