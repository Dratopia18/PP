trait Nat
case object Zero extends Nat
case class Succ(x: Nat) extends Nat

def add(x: Nat, y: Nat): Nat =
  y match {
    case Zero => x
    case Succ(n) => add(Succ(x), n)
  }
//ex 1
def realrealtrycatch(t: => Option[Int], c: => Int): Int = {
  try {
    t match {
      case Some(x) => x
      case None => c
    }
  } catch {
    case e: Exception => c
  }
}
//ex 2
def toNatOpt(x: Int): Option[Nat] = {
  if (x < 0) None
  else if (x == 0) Some(Zero)
  else toNatOpt(x - 1).map(Succ)
}
toNatOpt(5)
// ex 3
def addOpt(x: Option[Nat], y: Option[Nat]): Option[Nat] =
  (x, y) match {
    case (Some(a), Some(b)) => Some(add(a, b))
    case _ => None
}
addOpt(toNatOpt(5), toNatOpt(3))