def takeP(p: Int => Boolean)(l: List[Int]): List[Int] = {
  l match {
    case Nil => Nil
    case x :: xs => if (p(x)) x :: takeP(p)(xs) else takeP(p)(xs)
  }
}
takeP(_ % 2 == 0)(List(1,2,3,4,5,6))