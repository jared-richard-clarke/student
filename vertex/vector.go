// Go
package main

import (
	"fmt"
	"math"
)

type Vector struct {
	x, y float64
}

func (v Vector) Magnitude() float64 {
	return math.Sqrt(v.x*v.x + v.y*v.y)
}

func main() {
	
	v1 := Vector{3, 4}
	v2 := Vector{1, 2}
	
	fmt.Println(v1.Magnitude()) // -> 5
	fmt.Println(v2.x)        // -> 1
}
