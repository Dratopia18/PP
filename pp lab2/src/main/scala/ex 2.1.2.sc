def doubler(): Int => Int = {
  def double(x: Int): Int = x * 2
  double
}

doubler()(5)