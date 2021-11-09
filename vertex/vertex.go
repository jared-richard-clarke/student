// Go
package main

import (
	"fmt"
	"math"
)

type Vertex struct {
	x, y float64
}

func (vertex Vertex) Origin() float64 {
	return math.Sqrt(vertex.x*vertex.x + vertex.y*vertex.y)
}

func main() {
	
	v1 := Vertex{3, 4}
	v2 := Vertex{1, 2}
	
	fmt.Println(v1.Origin()) // -> 5
	fmt.Println(v2.x)        // -> 1
}
