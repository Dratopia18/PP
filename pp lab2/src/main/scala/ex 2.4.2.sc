def shiftOX(line: Double => Double, delta_x: Double): Double => Double = {
  (x: Double) => line(x - delta_x)
}
shiftOX((x: Double) => x * x, 1)(2)