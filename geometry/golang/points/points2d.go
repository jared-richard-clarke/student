package points

import (
	"didact/geometry/golang/matrices"
	"didact/geometry/golang/vectors"
)

type Pt2 [2]float64

func (p Pt2) Add(v vectors.Vec2) Pt2 {
	for i := range p {
		p[i] += v[i]
	}
	return p
}

func (p1 Pt2) Sub(p2 Pt2) vectors.Vec2 {
	var v vectors.Vec2
	for i := range v {
		v[i] = p1[i] - p2[i]
	}
	return v
}

func (p1 Pt2) Distance(p2 Pt2) float64 {
	return p2.Sub(p1).Mag()
}

func (p1 Pt2) Lerp(t float64, p2 Pt2) Pt2 {
	for i := range p1 {
		p1[i] += (p2[i] - p1[i]) * t
	}
	return p1
}

func (p Pt2) Transform(m matrices.Mat3) Pt2 {
	return Pt2{
		m[0]*p[0] + m[2]*p[1] + m[4],
		m[1]*p[0] + m[3]*p[1] + m[5],
	}
}
