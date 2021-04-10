(1 to 3).toList
  .map(_ + 1)
  .map(_ * 2)
  .map(_ + "!")

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
val future: Future[String] =
  Future(123)
    .map(_ + 1)
    .map(_ * 2)
    .map(_ + "!")
Await.result(future, 1.second)

val func1: Int => Double =
  (x: Int) => x.toDouble
val func2: Double => Double =
  (y: Double) => y * 2

func2(func1(1))
(func1 andThen func2)(1)

import cats.instances.function._
import cats.syntax.functor._
(func1 map func2)(1)

val func = ((x:Int) => x.toDouble)
  .map(_ + 1)
  .map(_ * 2)
  .map(_ + "!")
func(123)

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]
}


val list1 = List(1,2,3)

import cats.instances.list._
val list2 = cats.Functor[List].map(list1)(_ * 2)

import cats.instances.option._
val option2 = cats.Functor[Option].map(Option(1,2,3))(_.toString)

val liftedFunc = cats.Functor[Option].lift(func)
liftedFunc(Option(1))

((a:Int) => a + 1)
  .andThen((a:Int) => a * 2)
  .andThen((a:Int) => a + "!")(123)

def doMath[F[_]](start: F[Int])(implicit f: cats.Functor[F]): F[Int] =
  start.map(_ + 1 * 2)

doMath(Option(20))
doMath(List(1,2,3))

final case class Box[A](value: A)
val box = Box[Int](123)

implicit val boxFunctor: cats.Functor[Box] =
  new cats.Functor[Box] {
    override def map[A, B](fa: Box[A])(f: A => B): Box[B] =
      Box(f(fa.value))
  }
box.map(_ + 1)

implicit def futureFunctor(implicit ec: ExecutionContext): cats.Functor[Future] =
  new cats.Functor[Future] {
    override def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa.map(f)(ec)
  }

sealed trait Tree[+A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]

implicit val treeFunctor: cats.Functor[Tree] =
  new cats.Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Branch(l,r) => Branch(map(l)(f), map(r)(f))
      case Leaf(v) => Leaf(f(v))
    }
  }
cats.Functor[Tree].map(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))))(_ + 1)

trait Printable[A] { self =>
  def format(value: A): String
  def contramap[B](f: B => A): Printable[B] = new Printable[B] {
    override def format(value: B): String = self.format(f(value))
  }
}

def format[A](value: A)(implicit p: Printable[A]): String = p.format(value)

implicit val stringPrintable: Printable[String] =
  (value: String) => s""""${value}""""

implicit val booleanPrintable: Printable[Boolean] =
  (value: Boolean) => if (value) "yes" else "no"

//implicit def boxPrintable[A]: Printable[Box[A]] =
//  (value: Box[A]) => "[" + value.value + "]"

implicit def boxPrintable[A](implicit p: Printable[A]) =
  p.contramap[Box[A]](_.value)

format("hello")
format(true)
format(Box("hello"))
format(Box(true))

trait Codec[A] { self =>
  def encode(value: A): String
  def decode(value: String): A
  def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {
    override def encode(value: B): String = self.encode(enc(value))
    override def decode(value: String): B = dec(self.decode(value))
  }
}

def encode[A](value: A)(implicit c: Codec[A]): String =
  c.encode(value)

def decode[A](value: String)(implicit c: Codec[A]): A =
  c.decode(value)

implicit val stringCodec: Codec[String] =
  new Codec[String] {
    override def encode(value: String): String = value
    override def decode(value: String): String = value
  }

implicit val intCodec: Codec[Int] = stringCodec.imap(_.toInt, _.toString)
implicit val booleanCodec: Codec[Boolean] = stringCodec.imap(_.toBoolean, _.toString)
implicit val booleanCodec: Codec[Double] = stringCodec.imap(_.toDouble, _.toString)

case class Box[A](value: A)

implicit def boxCodec[A](implicit c: Codec[A]) =
  new Codec[Box[A]] {
    override def encode(value: Box[A]): String = value.value.toString
    override def decode(value: String): Box[A] = Box[A](c.decode(value))
  }

encode(123.4)
decode[Double]("123.4")
encode(Box(123.4))
decode[Box[Double]]("123.4")

trait Contravariant[F[_]] {
  def contramap[A,B](fa: F[A])(f: B => A): F[B]
}

trait Invariant[F[_]] {
  def imap[A,B](fa: F[A])(f: A => B)(g: B => A): F[B]
}

import cats.instances.string._
val showString = cats.Show[String]
val showSymbol = cats.Contravariant[cats.Show].contramap(showString)((sym: Symbol) => s"'${sym.name}")
showSymbol.show(Symbol("dave"))

import cats.syntax.contravariant._
showString.contramap[Symbol](_.name).show(Symbol("dave"))

import cats.syntax.invariant._
import cats.syntax.semigroup._

implicit val symbolMonoid: cats.Monoid[Symbol] =
  cats.Monoid[String].imap(Symbol.apply)(_.name)

cats.Monoid[Symbol].empty
Symbol("a") |+| Symbol("few") |+| Symbol("words")

val fun1 = (x: Int) => x.toDouble
val fun2 = (y: Double) => y * 2
import cats.instances.function._
import cats.syntax.functor._
val fun3a = fun1.map(fun2)
val fun3b = fun1.andThen(fun2)
val fun3c = fun2.compose(fun1)