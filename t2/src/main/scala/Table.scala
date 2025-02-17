type Row = Map[String, String]
type Tabular = List[Row]

case class Table (tableName: String, tableData: Tabular) {

  override def toString: String = {
    val head = data.head.keys.mkString(",")
    val rows = data.map(_.values.mkString(",")).mkString("\n")
    head + "\n" + rows
  }

  def insert(row: Row): Table = {
    if (tableData.contains(row)) {
      this
    } else {
      Table(tableName, tableData :+ row)
    }
  }

  def delete(row: Row): Table = {
    val newData = tableData.filterNot(_ == row)
    Table(tableName, newData)
  }

  def sort(column: String): Table = {
    val newData = tableData.sortBy(_(column))
    Table(tableName, newData)
  }

  def update(f: FilterCond, updates: Map[String, String]): Table = {
    val newData = tableData.map { row =>
      f.eval(row) match {
        case Some(true) => row ++ updates
        case _ => row
      }
    }
    Table(tableName, newData)
  }

  def filter(f: FilterCond): Table = {
    val newData = tableData.zip(tableData.map(f.eval)).filter(_._2.contains(true)).map(_._1)
    Table(tableName, newData)
  }

  def select(columns: List[String]): Table = {
    val newData = tableData.map(row => row.view.filterKeys(columns.contains).toMap)
    Table(tableName, newData)
  }

  def header: List[String] = data.head.keys.toList
  def data: Tabular = tableData
  def name: String = tableName
}

object Table {
  def apply(name: String, s: String): Table = {
    val lines = s.split("\n").toList
    val header = lines.head.split(",").toList
    val data = lines.tail.map(_.split(",").toList).map(header.zip(_).toMap)
    Table(name, data)
  }
}

extension (table: Table) {
  def todo(i: Int): Row = table.tableData(i) // Implement indexing here, find the right function to override
}
