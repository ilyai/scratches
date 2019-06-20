object EMail {
  // The injection method (optional)
  def apply(user: String, domain: String) = user + '@' + domain
  // The extraction method (mandatory)
  def unapply(str: String): Option[(String, String)] = {
    val parts = str split "@"
    if (parts.length == 2) Some(parts(0), parts(1)) else None
  }
}

EMail.unapply("Jehn@epfl.ch") match {
  case Some((u, d)) => EMail.apply(u, d)
}

"Jehn@epfl.ch" match {
  case EMail(user, domain) =>
    println(s"user:$user domain:$domain")
}

object Twice {
  def apply(s: String): String = s + s
  def unapply(s: String): Option[String] = {
    val length = s.length / 2
    val half = s.substring(0, length)
    if (half == s.substring(length)) Some(half) else None
  }
}

object UpperCase {
  def unapply(s: String): Boolean = s.toUpperCase == s
}

def userTwiceUpper(s: String) = s match {
  case EMail(Twice(x @ UpperCase()), domain) =>
    s"match: $x in domain $domain"
  case _ =>
    "no match"
}

userTwiceUpper("DIDI@hotmail.com")
userTwiceUpper("DIDO@hotmail.com")
userTwiceUpper("didi@hotmail.com")

object Domain {
  // The injection method (optional)
  def apply(parts: String*): String =
    parts.reverse.mkString(".")
  // The extraction method (mandatory)
  def unapplySeq(whole: String): Option[Seq[String]] =
    Some(whole.split("\\.").reverse)
}

def isTomInDotCom(s: String): Boolean = s match {
  case EMail("tom", Domain("com", _*)) => true
  case _ => false
}

isTomInDotCom("tom@sun.com")

object ExpandedEMail {
  def unapplySeq(email: String): Option[(String, Seq[String])] = {
    val parts = email split "@"
    if (parts.length == 2) Some(parts(0), parts(1).split("\\.").reverse) else None
  }
}

val s = "tom@support.epfl.ch"
val ExpandedEMail(name, topdom, subdoms @ _*) = s


object List {
  def apply[T](elems: T*) = elems.toList
  def unapplySeq[T](x: List[T]): Option[Seq[T]] = Some(x)
}

import scala.util.matching.Regex
//val Decimal = new Regex("(-)?(\\d+)(\\.\\d*)?")
//val Decimal = new Regex("""(-)?(\d+)(\.\d*)?""")

val Decimal = """(-)?(\d+)(\.\d*)?""".r

val input = "for -1.0 to 99 by 3"

for (s <- Decimal findAllIn input)
  println(s)

Decimal findPrefixOf input

val Decimal(sign, integerpart, decimalpart) = "-1.23"

for (Decimal(s, i, d) <- Decimal findAllIn input)
  println("sign: " + s + ", integer: " + i + ", decimal: " + d)

