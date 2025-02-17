def apply(n: Int, f: Int => Int): Int = {
  f(n)
}
apply(5, (x: Int) => x * x * x)