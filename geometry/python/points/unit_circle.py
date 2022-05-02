from points import point

# unit_circle[number] -> tuple
# For a series of angles, returns the corresponding coordinates of a unit circle.
# Implemented as a dictionary whose keys are integers and values are tuples.
# unit_circle[90]  -> (0, 1)
# unit_circle[180] -> (-1, 0)

unit_circle = {
    0:   point(1, 0),
    30:  point(0.866, 0.5),
    45:  point(0.707, 0.707),
    60:  point(0.5, 0.866),
    90:  point(0, 1),
    120: point(-0.5, 0.866),
    135: point(-0.707, 0.707),
    150: point(-0.866, 0.5),
    180: point(-1, 0),
    210: point(-0.866, -0.5),
    225: point(-0.707, -0.707),
    240: point(-0.5, -0.866),
    270: point(0, -1),
    300: point(0.5, -0.866),
    315: point(0.707, -0.707),
    330: point(0.866, -0.5),
}
