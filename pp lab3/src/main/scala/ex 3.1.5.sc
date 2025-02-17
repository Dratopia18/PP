def part(p: Int => Boolean)(l: List[Int]): (List[Int], List[Int]) = {
  l match {
    case Nil => (Nil, Nil)
    case x :: xs =>
      val (a, b) = part(p)(xs)
      if (p(x)) (x :: a, b) else (a, x :: b)
  }
}
part(_ % 2 == 0)(List(1,2,3,4,5,6))