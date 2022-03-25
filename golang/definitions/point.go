package definitions

import "math"

type Point struct {
	X, Y float64
}

func (p1 Point) Distance(p2 Point) float64 {
	return math.Hypot(p2.X-p1.X, p2.Y-p1.Y)
}

type Path []Point

func (p Path) Distance() float64 {
	sum := float64(0)
	for i := range p {
		if i > 0 {
			sum += p[i-1].Distance(p[i])
		}
	}
	return sum
}
