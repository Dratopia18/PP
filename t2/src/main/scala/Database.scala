case class Database(tables: List[Table]) {
  override def toString: String = tables.mkString("\n")

  def create(tableName: String): Database = {
    if (tables.exists(_.name == tableName)) {
      this
    } else {
      Database(tables :+ Table(tableName, List()))
    }
  }

  def drop(tableName: String): Database = {
    val newTables = tables.filterNot(_.name == tableName)
    Database(newTables)
  }

  def selectTables(tableNames: List[String]): Option[Database] = {
    val selectedTables = tables.filter(t => tableNames.contains(t.name))
    if (selectedTables.length == tableNames.length) {
      Some(Database(selectedTables))
    } else {
      None
    }
  }

  def join(table1: String, c1: String, table2: String, c2: String): Option[Table] = {
    val t1 = tables.find(_.name == table1)
    val t2 = tables.find(_.name == table2)
    if (t1.isEmpty || t2.isEmpty) {
      None
    } else {
      val rows1 = t1.get.tableData
      val rows2 = t2.get.tableData
      val column1 = t1.get.header
      val column2 = t2.get.header

      val commonRows = rows1.flatMap { row1 =>
        rows2.find(row2 => row1(c1) == row2(c2)) match {
          case Some(row2) =>
            val combinedRow = row1 ++ row2.view.filterKeys(_ != c2).map { case (k, v) =>
              if (row1.contains(k) && row2.contains(k) && row1(k) != row2(k)) {
                k -> s"${row1(k)};${row2(k)}"
              } else {
                k -> (if (v == "") row1(c1) else if (row1(c1) == "") v else v)
              }
            }
            Some(combinedRow)
          case None => None
        }
      }
      val unique1 = rows1.filterNot(row1 => rows2.exists(row2 => row1(c1) == row2(c2)))
        .map { u1 =>
          val matchingRow2 = rows2.find(row2 => row2(c2) == u1(c1))
          matchingRow2 match {
            case Some(row2) =>
              u1 ++ row2.view.filterKeys(_ != c2).map { case (k, v) =>
                k -> (if (v == "") v else "")
              }
            case None =>
              u1 ++ column2.filter(_ != c2).map { k =>
                k -> (if (k == c2) u1(c1) else u1.getOrElse(k, ""))
              }
          }
        }
      val unique2 = rows2.filterNot(row2 => rows1.exists(row1 => row1(c1) == row2(c2)))
        .map {u2 =>
          val updatedRow = if (u2.contains(c2)) {
            u2 - c2 + (c1 -> u2(c2))
          } else {
            u2
          }
          val matchingRow1 = rows1.find(row1 => row1(c1) == updatedRow(c1))
          matchingRow1 match {
            case Some(row1) =>
              updatedRow ++ row1.view.filterKeys(_ != c1).map { case (k, v) =>
                k -> (if (v == "") v else "")
              }
            case None =>
              updatedRow ++ column1.filter(_ != c1).map { k =>
                k -> updatedRow.getOrElse(k, "")
              }
          }
        }
      val combinedRows = commonRows ++ unique1 ++ unique2

      val resultTableName = s"${table1}_$table2"
      val resultTabularData = combinedRows.map(_.toMap)
      val resultTable = Table(resultTableName, resultTabularData)
      Some(resultTable)
    }
  }
  // Implement indexing here
  def apply(index: Int): Table = tables(index)
}
