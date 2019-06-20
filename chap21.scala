//val button = new JButton
//button.addActionListener(
//  new ActionListener {
//    def actionPerformed(event: ActionListener): Unit = {
//      println("pressed!")
//    }
//  }
//)

implicit def intToString(x: Integer): String = s"<${x.toString}>"

object Dollar {
  implicit def dollarToEuro(x: Dollar): Euro = ???
}
class Dollar
class Euro

object MyConversions {
  implicit def stringWrapper(s: String): IndexedSeq[Char] = ???
  implicit def intToString(x: Int): String = ???
}

import MyConversions.stringWrapper

implicit def doubleToInt(x: Double) = x.toInt
val i: Int = 3.5

class Rational(val n: Int, val d: Int) {
  def + (that: Rational): Rational = new Rational(n + that.n, d)

  override def toString: String = s"$n/$d"
}
implicit def intToRational(x: Int): Rational = new Rational(x, 1)
val oneHalf = new Rational(1,2)
3 + oneHalf

class PreferredPrompt(val preference: String)
class PreferredDrink(val preference: String)

object Greeter {
  def greet(name: String)(implicit prompt: PreferredPrompt, drink: PreferredDrink): Unit = {
    println(s"Welcome, $name. The system is ready.")
    println(prompt.preference)
  }
}

val bobsPrompt = new PreferredPrompt("relax> ")
val tea = new PreferredDrink("tea")
Greeter.greet("Bob")(bobsPrompt, tea)

object JoesPrefs {
  implicit val prompt = new PreferredPrompt("Yes, master> ")
  implicit val drink = new PreferredDrink("beer")
}

import JoesPrefs._
Greeter.greet("Joe")

def maxListUpBound[T <: Ordered[T]](elements: List[T]): T =
elements match {
  case List() =>
    throw new IllegalArgumentException("empty list!")
  case List(x) => x
  case x :: rest =>
    val maxRest = maxListUpBound(rest)
    if (x > maxRest) x
    else maxRest
}

def maxListImpParm[T](elements: List[T])(implicit orderer: T => Ordered[T]): T =
  elements match {
    case List() =>
      throw new IllegalArgumentException("empty list!")
    case List(x) => x
    case x :: rest =>
      val maxRest = maxListImpParm(rest)    // (orderer)
      if (x > maxRest) x      // orderer(x)
      else maxRest
  }

maxListImpParm(List(1,5,10,3))

def maxList[T <% Ordered[T]](elements: List[T]): T =
  elements match {
    case List() =>
      throw new IllegalArgumentException("empty list!")
    case List(x) => x
    case x :: rest =>
      val maxRest = maxList(rest)
      if (x > maxRest) x
      else maxRest
  }

def printLength(seq: Seq[Int]) = println(seq.length)

implicit def intToRange(i: Int) = 1 to i
//implicit def intToDigits(i: Int) = i.toString.toList.map(_.toInt)

//"abc" == "abc".reverse.reverse

//val chars2: List[Char] = "xyz"

//val chars3: List[Char] = wrapString("xyz")