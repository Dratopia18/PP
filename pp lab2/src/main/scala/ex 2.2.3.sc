import scala.annotation.tailrec

def foldWith (op: (Int,Int) => Int)(start: Int, stop: Int, f: Int => Int): Int = {
  @tailrec
  def tail_fold(crt: Int, acc: Int): Int  = {
    if (crt > stop) acc
    else tail_fold(crt + 1, op(f(crt), acc))
  }
  tail_fold(start, 0)
}

def foldMap(op: (Int,Int) => Int, f: Int => Int)(start: Int, stop: Int): Int = {
  foldWith(op)(start, stop, f)
}

foldMap(_ + _, (x: Int) => x * 2)(1, 10)

