import java.util.regex._
import scala.util.matching.Regex
//import fpinscala.testing._
//import fpinscala.testing.Prop._
import language.implicitConversions

trait Parsers[Parser[+_]] { self =>
  def run [A](p: Parser[A])(input: String): Either[ParseError, A]

  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  def defaultSucceed[A](a: A): Parser[A] =
    string("") map (_ => a)

  def succeed[A](a: A): Parser[A]

  def slice[A](p: Parser[A]): Parser[String]

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List())

  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  implicit def regex(r: Regex): Parser[String]

  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] =
    flatMap(p)(a => map(p2)(b => (a,b)))

  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
    for { a <- p; b <- p2 } yield f(a,b)

  def map[A,B](a: Parser[A])(f: A => B): Parser[B] =
    flatMap(a)(f andThen succeed)

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def attempt[A](p: Parser[A]): Parser[A]

  def skipL[B](p: Parser[Any], p2: => Parser[B]): Parser[B] =
    map2(slice(p), p2)((_,b) => b)

  def skipR[A](p: Parser[A], p2: => Parser[Any]): Parser[A] =
    map2(p, slice(p2))((a,_) => a)

  def opt[A](p: Parser[A]): Parser[Option[A]] =
    p.map(Some(_)) or succeed(None)

  def whitespace: Parser[String] = "\\s*".r

  def digits: Parser[String] = "\\d+".r

  def thru(s: String): Parser[String] = (".*?"+Pattern.quote(s)).r

  def quoted: Parser[String] = string("\"") *> thru("\"").map(_.dropRight(1))

  def escapedQuoted: Parser[String] =
    token(quoted label "string literal")

  def doubleString: Parser[String] =
    token("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r)

  def double: Parser[Double] =
    doubleString map (_.toDouble) label "double literal"

  def token[A](p: Parser[A]): Parser[A] =
    attempt(p) <* whitespace

  def sep[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] =
    sep1(p,p2) or succeed(List())

  def sep1[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] =
    map2(p, many(p2 *> p))(_ :: _)

  def opL[A](p: Parser[A])(op: Parser[(A,A) => A]): Parser[A] =
    map2(p, many(op ** p))((h,t) => t.foldLeft(h)((a,b) => b._1(a,b._2)))

  def surround[A](start: Parser[Any], stop: Parser[Any])(p: => Parser[A]) =
    start *> p <* stop

  def eof: Parser[String] =
    regex("\\z".r).label("unexpected trailing characters")

  def root[A](p: Parser[A]): Parser[A] =
    p <* eof

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
    def or[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def slice: Parser[String] = self.slice(p)

    def **[B](p2: => Parser[B]): Parser[(A,B)] =
      self.product(p,p2)

    def flatMap[B](f: A => Parser[B]): Parser[B] =
      self.flatMap(p)(f)

    def label(msg: String): Parser[A] = self.label(msg)(p)

    def *>[B](p2: => Parser[B]) = self.skipL(p, p2)
    def <*[B](p2: => Parser[Any]) = self.skipR(p, p2)
    def sep1(separator: Parser[Any]) = self.sep1(p, separator)
  }
}

case class Location(input: String, offset: Int = 0)

case class ParseError(stack: List[(Location,String)] = List())

object Parsers {
//  val P: Parsers
//  import P._
}