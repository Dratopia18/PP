def rev(l: List[Int]): List[Int] = {
  l.foldLeft(List[Int]())((acc, x) => x :: acc)
}
rev(List(1,2,3,4,5,6))

def rev2(l: List[Int]): List[Int] = {
  l match {
    case Nil => Nil
    case x :: xs => rev2(xs) ::: List(x)
  }
}
rev2(List(1,2,3,4,5,6))