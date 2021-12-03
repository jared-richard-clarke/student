// Go
package main

import (
	"fmt"
	"math"
)

type Vertex struct {
	x, y float64
}

func (v Vertex) Magnitude() float64 {
	return math.Sqrt(v.x*v.x + v.y*v.y)
}

func main() {
	
	v1 := Vertex{3, 4}
	v2 := Vertex{1, 2}
	
	fmt.Println(v1.Magnitude()) // -> 5
	fmt.Println(v2.x)        // -> 1
}
