import scala.collection.IterableOnce.iterableOnceExtensionMethods

def show[A](list: List[A]): String =
  list.foldLeft("Nil")((acc,a) => s"$a then $acc")
//  list.foldRight("Nil")((acc,a) => s"$a then $acc")

show(Nil)

show(List(1,2,3))

List(1,2,3).foldLeft(0)(_ - _)
List(1,2,3).foldRight(0)(_ - _)

List(1,2,3).foldLeft(Nil:List[Int])((a,b) => b :: a)
List(1,2,3).foldRight(List.empty[Int])(_ :: _)

def map[A,B](as: List[A])(f: A => B): List[B] =
  as.foldRight(List.empty[B])((a,acc) => f(a) :: acc)

def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
  as.foldRight(List.empty[B])((a,acc) => acc ::: f(a))

def filter[A](as: List[A])(f: A => Boolean): List[A] =
  as.foldRight(List.empty[A])((a,acc) => if (f(a)) a :: acc else acc)

def sum[A](nums: List[A])(implicit m: cats.Monoid[A]): A =
  nums.foldRight(m.empty)(m.combine)

map(List(1,2,3))(_ * 2)
flatMap(List(1,2,3))(a => List(a, a * 10, a * 100))
filter(List(1,2,3))(_ % 2 == 1)
import cats.instances.int._
sum(List(1,2,3))

import cats.Foldable
import cats.instances.list._

val ints = List(1,2,3)
Foldable[List].foldLeft(ints, 0)(_ + _)

import cats.instances.option._
val maybeInt = Option(123)
Foldable[Option].foldLeft(maybeInt, 10)(_ * _)

import cats.Eval
import cats.Foldable

def bigData = (1 to 10000).to(LazyList)
//bigData.foldRight(0L)(_ + _)

import cats.instances.lazyList._
val eval: Eval[Long] =
  Foldable[LazyList].foldRight(bigData, Eval.now(0L)) {
    (num,eval) => eval.map(_ + num)
  }
eval.value

Foldable[Option].nonEmpty(Option(42))
Foldable[List].find(List(1,2,3))(_ % 2 == 0)

import cats.instances.int._
Foldable[List].combineAll(List(1,2,3))

import cats.instances.string._
Foldable[List].foldMap(List(1,2,3))(_.toString)

import cats.instances.vector._
val ints2 = List(Vector(1,2,3), Vector(4,5,6))
(Foldable[List] compose Foldable[Vector]).combineAll(ints2)

import cats.syntax.foldable._
List(1,2,3).combineAll
List(1,2,3).foldMap(_.toString)