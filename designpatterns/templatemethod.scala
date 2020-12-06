abstract class QueryTemplate {
  def doQuery(dbName: String, querySpec: String): Unit = {
    println(formatConnect(dbName))
    println(formatSelect(querySpec))
  }
  def formatConnect(dbName: String): String
  def formatSelect(querySpec: String): String
}

class OracleQT extends QueryTemplate {
  override def formatConnect(dbName: String) = s"oracle connect $dbName"
  override def formatSelect(querySpec: String) = s"oracle select $querySpec"
}

class MicrosoftQT extends QueryTemplate {
  override def formatConnect(dbName: String) = s"microsoft connect $dbName"
  override def formatSelect(querySpec: String) = s"microsoft select $querySpec"
}

val qt = new OracleQT
qt.doQuery("demo", "* from users")