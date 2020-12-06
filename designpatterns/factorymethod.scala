abstract class QueryTemplate {
  def doQuery(dbName: String, querySpec: String): Unit = {
    val db = makeDatabase()
    println(db.formatConnect(dbName))
    println(db.formatSelect(querySpec))
  }
  def formatConnect(dbName: String): String
  def formatSelect(querySpec: String): String
  def makeDatabase(): QueryTemplate
}

class OracleQT extends QueryTemplate {
  override def formatConnect(dbName: String) = s"oracle connect $dbName"
  override def formatSelect(querySpec: String) = s"oracle select $querySpec"
  override def makeDatabase(): QueryTemplate = new OracleQT
}

class MicrosoftQT extends QueryTemplate {
  override def formatConnect(dbName: String) = s"microsoft connect $dbName"
  override def formatSelect(querySpec: String) = s"microsoft select $querySpec"
  override def makeDatabase(): QueryTemplate = new MicrosoftQT
}

val qt = new OracleQT
qt.doQuery("demo", "* from users")