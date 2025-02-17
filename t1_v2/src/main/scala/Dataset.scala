import scala.io.Source

class Dataset(m: List[List[String]]) {
  val data: List[List[String]] = m
  override def toString: String = {
    val head = data.head.mkString(",")
    val rows = data.tail.map(_.mkString(",")).mkString("\n")
    head + "\n" + rows
  }

  def selectColumn(col: String): Dataset = {
    val index = data.head.indexOf(col)
    val new_data = data.map(row => List(row(index)))
    new Dataset(new_data)
  }
  def selectColumns(cols: List[String]): Dataset = {
    val indices = cols.map(col => data.head.indexOf(col))
    val new_data = data.map(row => indices.map(i => row(i)))
    new Dataset(new_data)
  }

  def splitHelper(sortedData: List[List[String]], splitIndex: Int, index: Int = 1,
                  trainData: List[List[String]] = List(), testData: List[List[String]] =
                  List()): (List[List[String]], List[List[String]]) = {
    sortedData match {
      case Nil => (trainData, testData)
      case head :: tail if index % splitIndex == 0 => splitHelper(tail, splitIndex, index + 1, trainData :+ head, testData)
      case head :: tail => splitHelper(tail, splitIndex, index + 1, trainData, testData :+ head)
    }
  }

  def split(percentage: Double): (Dataset, Dataset) = {
    val header = data.head
    val sortedData = data.tail.sortBy(_.head)

    val splitIndex = (1 / percentage).ceil.toInt
    val (trainData, testData) = splitHelper(sortedData, splitIndex)

    (new Dataset(header :: testData), new Dataset(header :: trainData))
  }

  def size: Int = data.length - 1
  def getRows: List[List[String]] = data.tail
  def getHeader: List[String] = data.head
}

object Dataset {
  def apply(csv_filename: String): Dataset = {
    val data = Source.fromFile(csv_filename).getLines().toList
    val header = data.head.split(",").toList
    val rows = data.tail.map(_.split(",").toList)
    new Dataset(header :: rows)
  }
  def apply(ds: List[List[String]]): Dataset = new Dataset(ds)
}