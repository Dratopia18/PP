import scala.annotation.tailrec

def foldConditional(op: (Int,Int) => Int, p: Int => Boolean)(start: Int, stop: Int): Int = {
  @tailrec
  def tail_fold(crt: Int, acc: Int): Int = {
    if (crt > stop) acc
    else if (p(crt)) tail_fold(crt + 1,  op(acc, crt))
    else tail_fold(crt + 1, acc)
  }
  tail_fold(start, 0)
}

foldConditional(_ + _, _ % 2 == 0)(1, 10)