trait Player {}
case object One extends Player {
  override def toString: String = "X"
}
case object Two extends Player {
  override def toString: String = "0"
}
case object Empty extends Player {
  override def toString: String = "."
}

type Line = List[Player]
type BoardList = List[Line]

case class Board(b: BoardList) {
  override def toString: String = b.map(_.mkString).mkString("\n")
}

def makeBoard(s: String): Board = {
  def toPos(c: Char): Player =
    c match {
      case 'X' => One
      case '0' => Two
      case _ => Empty
    }
    val lines = s.split("\n").toList
    val board = lines.map(_.toList.map(toPos))
    Board(board)
}