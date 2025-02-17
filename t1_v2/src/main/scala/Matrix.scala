type Mat = List[List[Double]]

class Matrix(m: Option[List[List[Double]]]) {

  def transpose: Matrix = {
    data match {
      case Some(data) => {
        val transposeData = data.head.indices.toList.map(row => data.map(_(row)))
        Matrix(Some(transposeData))
      }
      case None => Matrix(None)
    }
  }
  def map(f: Double => Double): Matrix = {
    data match {
      case Some(d) => Matrix(Some(d.map(_.map(f))))
      case None => Matrix(None)
    }
  }
  def *(other: Matrix): Matrix =
    data match {
      case Some(d) => {
        other.data match {
          case Some(o) if d.head.length == o.length => {
            val result = d.map {
              row1 => o.transpose.map {
                row2 => row1.zip(row2).map {
                  case (a, b) => a * b
                }.sum
              }
            }
            Matrix(Some(result))
          }
          case _ => Matrix(None)
        }
      }
      case None => Matrix(None)
    }

  def ++(x: Double): Matrix = {
    data match {
      case Some(d) => Matrix(Some(d.map(_ :+ x)))
      case None => Matrix(None)
    }
  }
  def -(other: Matrix): Matrix = {
    data match {
      case Some(d) => {
        other.data match {
          case Some(o) if d.length == o.length && d.head.length == o.head.length => {
            val result = d.zip(o).map {
              case (row1, row2) => row1.zip(row2).map {
                case (a, b) => a - b
              }
            }
            Matrix(Some(result))
          }
          case _ => Matrix(None)
        }
      }
      case None => Matrix(None)
    }
  }

  def data: Option[Mat] = m
  def height: Option[Int] = Some(data.get.length)
  def width: Option[Int] = Some(data.get.head.length)
  override def toString: String = data match {
    case Some(d) => d.map(_.mkString(" ")).mkString("\n")
    case None => "Matrix is empty"
  }
}

object Matrix {
  def apply(data: Mat): Matrix = new Matrix(Some(data))
  def apply(data: Option[Mat]): Matrix = new Matrix(data)
  def apply(dataset: Dataset): Matrix = {
    val data = dataset.data.tail.map(_.map(_.toDouble))
    data match {
      case d if d.nonEmpty => Matrix(Some(d))
      case _ => Matrix(None)
    }
  }
}
