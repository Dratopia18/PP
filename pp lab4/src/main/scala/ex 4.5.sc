type Matrix = List[List[Int]]
//ex 1
def scalarProd(m: Matrix)(v: Int): Matrix = m.map(_.map(_ *v))

scalarProd(List(List(1,2,3), List(4,5,6), List(7,8,9)))(2)
//ex 2
def hJoin(m1: Matrix, m2: Matrix): Matrix = m1.zip(m2).map{
  case (l1, l2) => l1 ++ l2
}

hJoin(List(List(1,2,3), List(4,5,6), List(7,8,9)), List(List(10,11,12), List(13,14,15), List(16,17,18)))
// ex 3
def vJoin(m1: Matrix, m2: Matrix): Matrix = m1 ++ m2

vJoin(List(List(1,2,3), List(4,5,6), List(7,8,9)), List(List(10,11,12), List(13,14,15), List(16,17,18)))
// ex 4
def matSum(m1: Matrix, m2: Matrix): Matrix = m1.zip(m2).map{
  case (l1, l2) => l1.zip(l2).map{
    case (x, y) => x + y
  }
}

matSum(List(List(1,2,3), List(4,5,6), List(7,8,9)), List(List(10,11,12), List(13,14,15), List(16,17,18)))