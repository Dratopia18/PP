def take(n: Int, l: List[Int]): List[Int] = {
  l match {
    case Nil => Nil
    case x :: xs => if (n > 0) x :: take(n - 1, xs) else Nil
  }
}
take(3, List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))