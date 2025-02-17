import scala.annotation.tailrec

def foldWith (op: (Int,Int) => Int)(start: Int, stop: Int): Int = {
  @tailrec
  def tail_fold(crt: Int, acc: Int): Int  = {
    if (crt > stop) acc
    else tail_fold(crt + 1, op(crt, acc))
  }
  tail_fold(start, 0)
}

foldWith(_ + _)(1, 7)