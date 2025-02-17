import scala.annotation.tailrec

@tailrec
def atLeastkPattern(k: Int, l: List[Int]): Boolean = {
  l match {
    case Nil => k <= 0
    case _ :: xs => atLeastkPattern(k - 1, xs)
  }
}
atLeastkPattern(3, List(1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21))


@tailrec
def atLeastkPatternPred(pred: Int => Boolean)(k: Int, l: List[Int]): Boolean = {
  l match {
    case Nil => false
    case x :: xs => if (pred(x)) true else atLeastkPatternPred(pred)(k - 1, xs)
  }
}

atLeastkPatternPred(_ % 2 == 0)(3, List(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22))