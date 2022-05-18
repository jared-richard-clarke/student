package angles

import (
	"math"
)

type Degrees float64
type Radians float64

func DegreesToRadians(d Degrees) Radians {
	return Radians(d * (math.Pi / 180.0))
}

func RadiansToDegrees(r Radians) Degrees {
	return Degrees(r * (180 / math.Pi))
}
