
trait Abstract {
  type T
  def transform(x: T): T
  val initial: T
  var current: T
}

class Concrete extends Abstract {
  type T = String
  def transform(x: String) = x + x
  val initial = "hi"
  var current = initial
}

abstract class Fruit {
  val v: String
  def m: String
}

abstract class Apple extends Fruit {
  val v: String
  val m: String
}

abstract class BadApple extends Fruit {
//  def v: String
  def m: String
}

trait AbstractTime {
  var hour: Int
  var minute: Int
}

trait RationalTrait {
  val numerArg: Int
  val denomArg: Int
  require(denomArg != 0)
}

val x = 2

//new RationalTrait {
//  val numerArg = 1 * x
//  val denomArg = 2 * x
//}

new {
  val numerArg = 1 * x
  val denomArg = 2 * x
} with RationalTrait

object twoThirds extends {
  val numerArg = 2
  val denomArg = 3
} with RationalTrait

class RationalClass(n: Int, d: Int) extends {
  val numerArg = n
  val denomArg = d
} with RationalTrait {
  def + (that: RationalClass) = new RationalClass(
    numerArg * that.denomArg + that.numerArg * denomArg,
    denomArg * that.denomArg
  )
}

object Demo {
  val x = {
    println("initializing x")
    "done"
  }
}

Demo
Demo.x

trait LazyRationalTrait {
  val numerArg: Int
  val denomArg: Int
  lazy val numer = numerArg / g
  lazy val denom = denomArg / g

  override def toString: String = numer + "/" + denom

  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)

  private lazy val g = {
    require(denomArg != 0)
    gcd(numerArg, denomArg)
  }
}

new LazyRationalTrait {
  override val denomArg: Int = 1 * x
  override val numerArg: Int = 2 * x
}

class Food

abstract class Animal {
  type SuitableFood <: Food
  def eat(food: SuitableFood)
}

class Grass extends Food
class Cow extends Animal {
  type SuitableFood = Grass
  override def eat(food: Grass): Unit = {

  }
}

class Fish extends Food
val bessy: Animal = new Cow
//bessy eat (new Fish)

class DogFood extends Food
class Dog extends Animal {
  type SuitableFood = DogFood
  override def eat(food: DogFood) {}
}

val lassie = new Dog
//lassie eat (new bessy.SuitableFood)

val bootsie = new Dog
lassie eat (new bootsie.SuitableFood)

class Outer {
  class Inner
}

val o1 = new Outer
val o2 = new Outer

new o1.Inner
//new Outer#Inner

//Animal { type SuitableFood = Grass }

class Pasture {
  var animals: List[Animal { type SuitableFood = Grass }] = Nil
}

def using[T <: { def close(): Unit }, S](obj: T)(operation: T => S) = {
  val result = operation(obj)
  obj.close()
  result
}

object Color extends Enumeration {
  val Red = Value
  val Green = Value
  val Blue = Value
}

object Color2 extends Enumeration {
  val Red, Green, Blue = Value
}

import Color._

object Direction extends Enumeration {
  val North = Value("North")
  val South = Value("South")
  val West = Value("West")
  val East = Value("East")
}

Direction.values

Direction.North.id
Direction.South.id

Direction(1)

abstract class Currency {
  val amount: Long
  def designation: String

  override def toString: String = amount + " " + designation
  def + (that: Currency): Currency = ???
  def * (x: Double): Currency = ???
}

new Currency {
  override def designation = "USD"
  override val amount: Long = 79L
}





abstract class Euro extends Currency {
  override def designation = "Euro"

}

abstract class AbstractCurrency {
  type Currency <: AbstractCurrency
  val amount: Long
  def designation: String
  def make(amount: Long): Currency

  override def toString: String = amount + " " + designation
  def + (that: Currency): Currency = make(this.amount + that.amount)
  def * (x: Double): Currency = make((this.amount * x).toLong)
}


abstract class CurrencyZone {
  type Currency <: AbstractCurrency
  def make(x: Long): Currency
//  val CurrencyUnit: Currency
  abstract class AbstractCurrency {
//    type Currency <: AbstractCurrency
    val amount: Long
    def designation: String
//    def make(amount: Long): Currency

    override def toString: String = amount + " " + designation
    def + (that: Currency): Currency = make(this.amount + that.amount)
    def * (x: Double): Currency = make((this.amount * x).toLong)
  }
}

abstract class Dollar extends AbstractCurrency {
  type Currency = Dollar
  override def designation = "USD"
}

object US extends CurrencyZone {
  abstract class Dollar extends AbstractCurrency {
    def designation = "USD"
  }
  type Currency = Dollar
  def make(cents: Long) = new Dollar {
    val amount = cents
  }
  val Cent = make(1)
  val Dollar = make(100)
  val CurrencyUnit = Dollar
}

object Europe extends CurrencyZone {
  abstract class Euro extends AbstractCurrency {
    override def designation = "EUR"
  }
  type Currency = Euro
  def make(cents: Long) = new Euro {
    val amount = cents
  }
  val Cent = make(1)
  val Euro = make(100)
  val CurrentUnit = Euro
}

