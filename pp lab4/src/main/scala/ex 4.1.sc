import scala.annotation.tailrec

trait Nat
case object Zero extends Nat
case class Succ(x: Nat) extends Nat
//ex 1
@tailrec
def add(x: Nat, y: Nat): Nat =
  y match {
  case Zero => x
  case Succ(n) => add(Succ(x), n)
}
add(Succ(Succ(Zero)), Succ(Succ(Succ(Zero))))
//ex 2
def multiply(x: Nat, y: Nat): Nat =
  y match {
    case Zero => Zero
    case Succ(n) => add(x, multiply(x, n))
  }
multiply(Succ(Succ(Zero)), Succ(Succ(Succ(Zero))))
//ex 3
def toNat(x: Int): Nat = {
  @tailrec
  def toNat(x: Int, acc: Nat): Nat = {
    if (x == 0) acc
    else toNat(x - 1, Succ(acc))
  }
  toNat(x, Zero)
}
toNat(3)