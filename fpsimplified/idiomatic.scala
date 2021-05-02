trait Person {
  def name: String
  def age: Int
  override def toString: String = s"name: $name, age: $age"
}

val mary: Person = new Person {
  val name = "mary"
  val age = 22
}

def timer[A](blockOfCode: => A) = {
  val startTime = java.lang.System.nanoTime()
  val result = blockOfCode
  val stopTime = java.lang.System.nanoTime()
  val delta = stopTime - startTime
  (result, delta/1000000d)
}

val (result, time) = timer {
  Thread.sleep(1000)
  42
}

case class StringToInt(run: String => Int)

val stringToInt = StringToInt { s: String =>
  s.length
}

stringToInt.run("bananas")

trait Functor[A] {
  def map[B](f: A => B): Functor[B]
}

Some(2).map(i => i*i)

def f(a: Int): (Int,String) = (a,"f")
def g(a: Int): (Int,String) = (a,"g")
def h(a: Int): (Int,String) = (a,"h")

def bind(fun: Int => (Int,String), tup: (Int,String)): (Int,String) = {
  val (i, s) = fun(tup._1)
  (i, tup._2 + s)
}

val fResult = f(100)
val gResult = bind(g, fResult)
val hResult = bind(h, gResult)

class Wrapper[A] private (value: A) {
  def map[B](f: A => B): Wrapper[B] = new Wrapper(f(value))
  def flatMap[B](f: A => Wrapper[B]): Wrapper[B] = f(value)
  override def toString: String = value.toString
}

object Wrapper {
  def apply[A](value: A) = new Wrapper(value)
}

for {
  a <- Wrapper("a")
  b <- Wrapper("b")
  c <- Wrapper("c")
} yield a + b + c

for {
  a <- Wrapper(1)
  b <- Wrapper(2)
  c <- Wrapper(3)
} yield a + b + c

Wrapper(1).flatMap(a =>
  Wrapper(2).flatMap(b =>
    Wrapper(3).map(c =>
      a + b + c
    )
  )
)

case class Debuggable[A] (value: A, log: List[String]) {
  def map[B](f: A => B): Debuggable[B] = Debuggable(f(value), log)
  def flatMap[B](f: A => Debuggable[B]): Debuggable[B] = f(value) match {
    case Debuggable(value,l) => Debuggable(value, log ::: l)
  }

  override def toString: String =
    s"""
      |value: $value
      |
      |message:
      |${log.mkString("\n")}
      |""".stripMargin
}

def fd(a: Int) = Debuggable(a * 2, List(s"fd: $a * 2 = ${a*2}"))
def gd(a: Int) = Debuggable(a * 3, List(s"gd: $a * 3 = ${a*3}"))
def hd(a: Int) = Debuggable(a * 4, List(s"hd: $a * 4 = ${a*4}"))

for {
  a <- fd(100)
  b <- gd(a)
  c <- hd(b)
} yield c