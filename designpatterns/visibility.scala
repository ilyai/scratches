trait EncodedString {
  protected val string: String
  val separator: EncodedString.Separator.Value

  override def toString: String = string

  def toTokens = string.split(separator.toString).toList
}

object EncodedString {
  object Separator extends Enumeration {
    type Delimiter = Value
    val COMMA = Value(",")
    val TAB = Value("\t")
  }

  def apply(s: String, sep: Separator.Delimiter) = sep match {
    case Separator.COMMA => CSV(s)
    case Separator.TAB => TSV(s)
  }

  def unapply(es: EncodedString): Option[(String,Separator.Delimiter)] = Some((es.string, es.separator))
}

case class CSV(override val string: String) extends EncodedString {
  override val separator: EncodedString.Separator.Value = EncodedString.Separator.COMMA
}

case class TSV(override val string: String) extends EncodedString {
  override val separator: EncodedString.Separator.Value = EncodedString.Separator.TAB
}

import EncodedString._

def p(s: EncodedString) = {
  println(s"EncodedString: $s")
  s.toTokens.foreach(t => println("token: " + t))
}

val csv = EncodedString("Scala,is,great!", Separator.COMMA)
val tsv = EncodedString("Scala\tis\tgreat!", Separator.TAB)

p(csv)
p(tsv)