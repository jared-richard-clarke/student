package angles

import (
	"fmt"
	"math"
)

type Degrees float64
type Radians float64

func (d Degrees) String() string {
	return fmt.Sprintf("%f°", d)
}

func (r Radians) String() string {
	return fmt.Sprintf("%frad", r)
}

func DegreesToRadians(d Degrees) Radians {
	return Radians(d * (math.Pi / 180.0))
}

func RadiansToDegrees(r Radians) Degrees {
	return Degrees(r * (180 / math.Pi))
}
