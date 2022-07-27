// Provides units of measure and conversion functions for angles.
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

// Converts Degrees to Radians.
func DegreesToRadians(d Degrees) Radians {
	return Radians(d * (math.Pi / 180.0))
}

// Converts Radians to Degrees.
func RadiansToDegrees(r Radians) Degrees {
	return Degrees(r * (180 / math.Pi))
}
