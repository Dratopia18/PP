import scala.annotation.tailrec

def fact (n: Int): Int = {
  @tailrec
  def aux_fact(n: Int, acc: Int): Int =
    if (n == 0) acc
    else aux_fact(n-1, n*acc)
  aux_fact(n, 1)
}
fact(5)