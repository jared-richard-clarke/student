package definitions

type Point struct {
	X, Y float64
}

type Degrees int

var UnitCircle = map[Degrees]Point{
	0:   {1, 0},
	30:  {0.866, 0.5},
	45:  {0.707, 0.707},
	60:  {0.5, 0.866},
	90:  {0, 1},
	120: {-0.5, 0.866},
	135: {-0.707, 0.707},
	150: {-0.866, 0.5},
	180: {-1, 0},
	210: {-0.866, -0.5},
	225: {-0.707, -0.707},
	240: {-0.5, -0.866},
	270: {0, -1},
	300: {0.5, -0.866},
	315: {0.707, -0.707},
	330: {0.866, -0.5},
}
