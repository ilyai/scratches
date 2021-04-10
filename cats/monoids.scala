2 + 1
2 + 0
0 + 2
(1 + 2) + 3
1 + (2 + 3)

"One" ++ "two"
"" ++ "Hello"
"Hello" + ""
("One" ++ "Two") ++ "Three"
"One" ++ ("Two" ++ "Three")

trait Semigroup[A] {
  def combine(x: A, y: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {
  def apply[A](implicit monoid: Monoid[A]) = monoid
}

def associativeLaw[A](x: A, y: A, z: A)(implicit m: Monoid[A]): Boolean =
  m.combine(x, m.combine(y, z)) == m.combine(m.combine(x,y), z)

def identityLaw[A](x: A)(implicit m: Monoid[A]): Boolean =
  (m.combine(x, m.empty) == x) && (m.combine(m.empty, x) == x)

//import cats.Monoid
import cats.instances.int._
import cats.instances.string._
import cats.instances.option._

cats.Monoid.combine("Hi ", "there")
cats.Monoid.combine(32, 10)
cats.Monoid.combine(Option(22), Option(20))
cats.Monoid[String].empty

cats.Semigroup.combine("Hi ", "there")

import cats.syntax.semigroup._
val stringResult = "Hi " |+| "there" |+| cats.Monoid[String].empty
val intResult = 1 |+| 2

object SuperAdder {
  case class Order(totalCost: Double, quantity: Double)

  implicit val orderMonoid: cats.Monoid[Order] = new cats.Monoid[Order] {
    override def combine(x: Order, y: Order): Order = Order(x.totalCost+y.totalCost, x.quantity+y.quantity)
    override def empty: Order = Order(0,0)
  }

  def add(items: List[Option[Int]]): Int =
    cats.Monoid.combineAll(items).getOrElse(0)
  def addOrders(orders: List[Order]): Order =
    cats.Monoid.combineAll(orders)
}

SuperAdder.add(List(Some(1),Some(2),None))

val map1 = Map("a" -> 1, "b" -> 2)
val map2 = Map("a" -> 3, "b" -> 4)
import cats.instances.map._
map1 |+| map2

val tuple1 = ("hello", 123)
val tuple2 = ("world", 321)
import cats.instances.tuple._
tuple1 |+| tuple2

def addAll[A](values: List[A])(implicit monoid: cats.Monoid[A]): A =
  values.foldRight(monoid.empty)(_ |+| _)

addAll(List(1,2,3))
addAll(List(None,Some(1),Some(2)))