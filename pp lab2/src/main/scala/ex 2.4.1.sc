def shiftOY(line: Double => Double, delta_y: Double): Double => Double = {
  (x: Double) => line(x) + delta_y
}

shiftOY((x: Double) => x * x, 1)(3)