import scala.annotation.tailrec

def improve(xn: Double, a: Double): Double = (xn + a / xn) / 2

improve(1.0, 2.0)

def nth_guess(n: Int, a: Double): Double = {
  if (n == 0) a
  else nth_guess(n - 1, (a + a / nth_guess(n - 1, a)) / 2)
}

nth_guess(3, 16.0)

def acceptable(xn: Double, a: Double): Boolean = Math.abs(xn * xn - a) < 0.001

acceptable(4, 15)

def mySqrt(a: Double): Double = {
  def improve1(xn: Double): Double = (xn + a / xn) / 2
  def acceptable1(xn: Double): Boolean =  Math.abs(xn * xn - a) / Math.max(1.0, Math.abs(a)) < 0.001

  @tailrec
  def tailSqrt(estimate: Double): Double = {
    if (acceptable1(estimate)) estimate
    else tailSqrt(improve1(estimate))
  }

  tailSqrt(1.0)
}

mySqrt(25.0)
mySqrt(2.0e50)