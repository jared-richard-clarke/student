package main

import (
	"fmt"
)

type Rectangle struct {
	Length, Width float64
}
type Triangle struct {
	Opposite, Adjacent float64
}
type Shape interface {
	Area() float64
}

func (r Rectangle) Area() float64 {
	return r.Length * r.Width
}
func (t Triangle) Area() float64 {
	return (t.Opposite * t.Adjacent) / 2
}

func TotalArea(s ...Shape) float64 {
	total := float64(0)
	for _, v := range s {
		total += v.Area()
	}
	return total
}
func main() {
	rectangle := Rectangle{2, 5}
	square := Rectangle{2, 2}
	triangle := Triangle{3, 4}

	fmt.Println(rectangle.Area())                       // -> 10
	fmt.Println(square.Area())                          // -> 4
	fmt.Println(triangle.Area())                        // -> 6
	fmt.Println(TotalArea(rectangle, square, triangle)) // -> 20
}
