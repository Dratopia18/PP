trait Expr
case class Atom(a: Int) extends Expr
case class Add(e1: Expr, e2: Expr) extends Expr
case class Mult(e1: Expr, e2: Expr) extends Expr
// ex 1
def evaluate(e: Expr): Int = e match {
  case Atom(a) => a
  case Add(e1, e2) => evaluate(e1) + evaluate(e2)
  case Mult(e1, e2) => evaluate(e1) * evaluate(e2)
}

evaluate(Add(Atom(1), Mult(Atom(2), Atom(3))))
evaluate(Mult(Add(Atom(1), Atom(2)), Add(Atom(3), Atom(4))))

// ex 2
def simplify(e: Expr): Expr = e match {
  case Atom(a) => Atom(a)
  case Mult(a,  Add(b, c)) => Add(Mult(simplify(a), simplify(b)), Mult(simplify(a), simplify(c)))
  case Mult(Add(a, b), c) => Add(Mult(simplify(a), simplify(c)), Mult(simplify(b), simplify(c)))
  case Mult(a, b) => Mult(simplify(a), simplify(b))
  case Add(a, b) => Add(simplify(a), simplify(b))
}

simplify(Mult(Add(Atom(1), Atom(2)), Add(Atom(3), Atom(4))))
// ex 3
def optimize(e: Expr): Expr = e match {
  case Mult(a, Atom(1)) => optimize(a)
  case Mult(Atom(1), a) => optimize(a)
  case Add(a, Atom(0)) => optimize(a)
  case Add(Atom(0), a) => optimize(a)
  case Add(a, b) => Add(optimize(a), optimize(b))
  case Mult(a, b) => Mult(optimize(a), optimize(b))
  case Atom(a) => Atom(a)
}

optimize(Mult(Atom(1), Add(Atom(2), Atom(3))))