import scala.util.parsing.combinator._

class Arith extends JavaTokenParsers {
  def expr: Parser[Any] = term~rep("+"~term | "-"~term)
  def term: Parser[Any] = factor~rep("*"~factor | "/"~factor)
  def factor: Parser[Any] = floatingPointNumber | "("~expr~")"
}

object ParseExpr extends Arith {
  def test(input: String): Unit = {
    println(parseAll(expr, input))
  }
}

ParseExpr.test("2 * (3 + 7)")

object MyParsers extends RegexParsers {
  val ident: Parser[String] = """[a-zA-Z_]\w*""".r
}

class JSON extends JavaTokenParsers {
  def value: Parser[Any] = obj | arr | stringLiteral | floatingPointNumber | "null" | "true" | "false"
  def obj: Parser[Any] = "{"~repsep(member, ",")~"}"
  def arr: Parser[Any] = "["~repsep(value, ",")~"]"
  def member: Parser[Any] = stringLiteral ~ ":" ~ value
}

class JSON2 extends JavaTokenParsers {
  def value: Parser[Any] = (obj
      | arr
      | stringLiteral
      | floatingPointNumber
      | "null" ^^ (x => null)
      | "true" ^^ (x => true)
      | "false" ^^ (x => false)
    )

  def obj: Parser[Map[String, Any]] = "{"~> repsep(member, ",") <~"}" ^^ (Map() ++ _)
  def arr: Parser[List[Any]] = "["~> repsep(value, ",") <~"]"
  def member: Parser[(String, Any)] = stringLiteral ~ ":" ~ value ^^
  {
    case name ~ ":" ~value => (name, value)
  }
}

object ParseJSON extends JSON2 {
  def test(input: String): Unit = {
    println(parseAll(value, input))
  }
}

ParseJSON.test("""{"foo":123}""")