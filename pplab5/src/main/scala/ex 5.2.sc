import scala.language.implicitConversions
//ex 5.2.1

trait BoolExpr

case object True extends BoolExpr
case object False extends BoolExpr
case class Symbol(name: String) extends BoolExpr
case class And(left: BoolExpr, right: BoolExpr) extends BoolExpr
case class Or(left: BoolExpr, right: BoolExpr) extends BoolExpr
case class Not(expr: BoolExpr) extends BoolExpr

//ex 5.2.2

implicit def boolToExpr(b: Boolean): BoolExpr = if (b) True else False

//ex 5.2.3
extension (e: BoolExpr) {
  def &&(other: BoolExpr): BoolExpr = And(e, other)
  def ||(other: BoolExpr): BoolExpr = Or(e, other)
}

extension (e: BoolExpr) {
  def unary_~ : BoolExpr = Not(e)
}

//ex 5.2.4