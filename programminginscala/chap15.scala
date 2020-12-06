sealed abstract class Expr
case class Var(name: String) extends Expr
case class Number(num: Double) extends Expr
case class BinOp(operator: String, left: Expr, right: Expr) extends Expr

val v = Var("x")
val op = BinOp("+", Number(1), v)

op.right == Var("x")

op.copy(operator = "-")


def simplifyTop(expr: Expr): Expr = expr match {
  case BinOp("+", e, Number(0)) => e
  case _ => expr
}

op match {
  case BinOp(_, _, _) =>
    println(s"$op is a binary operation")
  case _ =>
}

op match {
  case BinOp(_, _, _) =>
    println(s"$op is a binary operation")
  case somethingElse =>
}

import math.{E, Pi}
val pi = math.E
E match {
  case `pi` => "strange math"
  case _ => "OK"
}

//op match {
//  case List(0, _*) => println("found it")
//  case _ =>
//}

def tupleDemo(expr: Any) =
  expr match {
    case (a, b, c) => println(s"matched $a $b $c")
    case _ =>
  }


tupleDemo(("a", "3", "-tuple"))

def generalSize(x: Any) = x match {
  case s: String => s.length
  case m: Map[_, _] => m.size
  case _ => -1
}

generalSize("abc")
generalSize(Map(1 -> 'a', 2 -> 'b'))
generalSize(math.Pi)

val x = ""
if (x.isInstanceOf[String]) {
  val s = x.asInstanceOf[String]
  s.length
}

def isIntIntMap(x: Any) = x match {
  case m: Map[Int, Int] => true
  case _ => false
}

isIntIntMap(Map(1 -> 1))
isIntIntMap(Map("abc" -> "abc"))

def isStringArray(x: Any) = x match {
  case a: Array[String] => "yes"
  case _ => "no"
}

isStringArray(Array("abc"))
isStringArray(Array(1,2,3))

op match {
  case BinOp("+", e @ Number(2), Number(0)) => e
  case _ => None
}

def simplifyAdd(e: Expr) = e match {
  case BinOp("+", x, y) if x == y =>
    BinOp("*", x, Number(2))
  case _ => e
}

def simplifyAll(expr: Expr): Expr = expr match {
  case BinOp("+", e, Number(0)) =>
    simplifyAll(e)
  case BinOp("*", e, Number(1)) =>
    simplifyAll(e)
  case BinOp(op, l, r) =>
    BinOp(op, simplifyAll(l), simplifyAll(r))
  case _ => expr
}


def describe(e: Expr): String = (e: @unchecked) match {
  case Number(_) => "a number"
  case Var(_) => "a variable"
}

val capitals = Map(
  "France" -> "Paris",
  "Japan" -> "Tokyo"
)

capitals get "France"

capitals get "North Pole"

def show(x: Option[String]) = x match {
  case Some(s) => s
  case None => "?"
}

show(capitals get "Japan")

val myTuple = (123, "abc")

val (number, string) = myTuple

val exp  = new BinOp("*", Number(5), Number(1))
val BinOp(op2, left, right) = exp

val withDefault: Option[Int] => Int = {
  case Some(x) => x
  case None => 0
}

withDefault(Some(10))
withDefault(None)

val second: List[Int] => Int = {
  case x ::  y :: _ => y
}

second(List(5,6,7))
//second(List())


val second2: PartialFunction[List[Int], Int] = {
  case x ::  y :: _ => y
}

second2.isDefinedAt(List(5,6,7))
second2.isDefinedAt(List())

val pf = new PartialFunction[List[Int], Int] {
  def apply(xs: List[Int]) = xs match {
    case x :: y :: _ => y
  }

  override def isDefinedAt(xs: List[Int]) = xs match {
    case x :: y :: _ => true
    case _ => false
  }
}

pf.isDefinedAt(List(5,6,7))
pf.isDefinedAt(List())

for ((country, city) <- capitals) println(s"the capital of ${country} is ${city}")

val results = List(
  Some("apple"),
  None,
  Some("orange")
)

for (Some(fruit) <- results) println(fruit)

val expr = BinOp("+",
  BinOp("*",
    BinOp("+", Var("x"), Var("y")),
    Var("z")),
  Number(1))


class ExprFormatter {
  // Constants operators in groups of increasing precedence
  private val opGroups =
    Array(
      Set("|", "||"),
      Set("&", "&&"),
      Set("^"),
      Set("==", "!="),
      Set("<", "<=", ">", ">="),
      Set("+", "-"),
      Set("*", "%")
    )

  // A mapping from operators to their precedence
  private val precedence = {
    val assocs =
      for {
        i <- 0 until opGroups.length
        op <- opGroups
      } yield op -> i
    assocs.toMap
  }

  private val unaryPrecedence = opGroups.length
  private val gractionPrecedence = -1

  private def format(e: Expr, enclPrec: Int) = ???
}