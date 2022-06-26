package matrices

import "math"

type Mat3 struct {
	XX, YX,
	XY, YY,
	X0, Y0 float64
}

func Identity() Mat3 {
	return Mat3{
		1, 0,
		0, 1,
		0, 0,
	}
}

func Translate(x, y float64) Mat3 {
	return Mat3{
		1, 0,
		0, 1,
		x, y,
	}
}

func Scale(x, y float64) Mat3 {
	return Mat3{
		x, 0,
		0, y,
		0, 0,
	}
}

func Rotate(angle float64) Mat3 {
	c := math.Cos(angle)
	s := math.Sin(angle)
	return Mat3{
		c, s,
		-s, c,
		0, 0,
	}
}

func Shear(x, y float64) Mat3 {
	return Mat3{
		1, y,
		x, 1,
		0, 0,
	}
}

func (a Mat3) Multiply(b Mat3) Mat3 {
	return Mat3{
		a.XX*b.XX + a.YX*b.XY,
		a.XX*b.YX + a.YX*b.YY,
		a.XY*b.XX + a.YY*b.XY,
		a.XY*b.YX + a.YY*b.YY,
		a.X0*b.XX + a.Y0*b.XY + b.X0,
		a.X0*b.YX + a.Y0*b.YY + b.Y0,
	}
}

func (a Mat3) Translate(x, y float64) Mat3 {
	return Translate(x, y).Multiply(a)
}

func (a Mat3) Scale(x, y float64) Mat3 {
	return Scale(x, y).Multiply(a)
}

func (a Mat3) Rotate(angle float64) Mat3 {
	return Rotate(angle).Multiply(a)
}

func (a Mat3) Shear(x, y float64) Mat3 {
	return Shear(x, y).Multiply(a)
}
