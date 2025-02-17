import scala.collection.immutable.Map

object Queries {

  def killJackSparrow(t: Table): Option[Table] = queryT(Some(t), "FILTER", Field("name", _ != "Jack"))

  def insertLinesThenSort(db: Database): Option[Table] = queryT(queryT(queryT(queryT(queryT(Some(queryDB((queryDB(Some(db), "CREATE", "Inserted Fellas"), "SELECT", List("Inserted Fellas"))).get.tables.head), "INSERT", List(Map("name" -> "Ana", "age" -> "93", "CNP" -> "455550555"))), "INSERT", List(Map("name" -> "Diana", "age" -> "33", "CNP" -> "255532142"))), "INSERT", List(Map("name" -> "Tatiana", "age" -> "55", "CNP" -> "655532132"))), "INSERT", List(Map("name" -> "Rosmaria", "age" -> "12", "CNP" -> "855532172"))), "SORT", "age")
  
  def youngAdultHobbiesJ(db: Database): Option[Table] =  queryT(queryT(Some(queryDB(Some(db), "JOIN", "People", "name", "Hobbies", "name").get.tables.head), "FILTER", Field("age", s => s.matches("\\d+") && s.toInt < 25) && Field("name", _.startsWith("J")) && Field("hobby", _ != "")), "EXTRACT", List("name", "hobby"))
}
