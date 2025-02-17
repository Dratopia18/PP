import scala.annotation.tailrec

def sumSquares(n: Int): Int = {
  if (n == 0) 0
  else n * n + sumSquares(n-1)
}
sumSquares(4)

def tailSumSquares(n: Int): Int = {
  @tailrec
  def auxSumSquares(n: Int, acc: Int): Int = {
    if (n == 0) acc
    else auxSumSquares(n-1, n * n + acc)
  }
  auxSumSquares(n, 0)
}

tailSumSquares(4)