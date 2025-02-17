def trycatch(t: Int => Int, c: Int => Int)(x: Int): Int = {
  if (t(x) == 0) c(x)
  else t(x)
}

trycatch((x: Int) => x + 1, (x: Int) => x - 1)(1)