# Python 3
import math

class Vertex:
	def __init__(self, x, y):
		self.x = x
		self.y = y
	def origin(self):
		return math.sqrt((self.x ** 2) + (self.y ** 2))

v1 = Vertex(3, 4)
v2 = Vertex(1, 2)

v1.origin() # -> 5.0
v2.x        # -> 1
