import scala.annotation.tailrec

@tailrec
def drop(n: Int, l: List[Int]): List[Int] = {
  l match {
    case Nil => Nil
    case _ :: xs => if (n > 0) drop(n - 1, xs) else l
  }
}
drop(3, List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))