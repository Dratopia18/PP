def intersect(line1: Double => Double, line2: Double => Double)(start: Int, stop: Int): Boolean = {
  def checkIntersection(x: Double): Boolean = {
    if (x > stop || x < start) false
    else if (line1(x) == line2(x)) true
    else checkIntersection(x + 1)
  }
  checkIntersection(start)
}

intersect(x => 3 * x + 1, x => 2 * x + 3)(1, 5)
intersect(x => 2 * x + 3, x => -x + 8)(1, 5)