import cats.Semigroup
import cats.data.NonEmptyChain
import cats.implicits.catsSemigroupalForMonoid
import cats.syntax.either._

def parseInteger(str: String): Either[String, Int] =
  Either.catchOnly[NumberFormatException](str.toInt).leftMap(_ => s"Could not read $str")

for {
  a <- parseInteger("a")
  b <- parseInteger("b")
  c <- parseInteger("c")
} yield (a + b + c)

//trait Semigroupal[F[_]] {
//  def product[A,B](fa: F[A], fb: F[B]): F[(A,B)]
//}

import cats.Semigroupal
import cats.instances.option._

Semigroupal[Option].product(Some(123), Some("abc"))
Semigroupal[Option].product(None, Some("abc"))
Semigroupal[Option].product(Some(123), None)

import cats.instances.option._
Semigroupal.tuple3(Option(1), Option(2), Option(3))
Semigroupal.tuple3(Option(1), Option(2), Option.empty[Int])

Semigroupal.map3(Option(1), Option(2), Option(3))(_ + _ + _)
Semigroupal.map2(Option(1), Option.empty[Int])(_ + _)

import cats.instances.option._
import cats.syntax.apply._

(Option(123), Option("abc")).tupled
(Option(123), Option("abc"), Option(true)).tupled

case class Cat(name: String, born: Int, color: String)

(
  Option("Garfield"),
  Option(1978),
  Option("Orange & black")
).mapN(Cat.apply)

val add: (Int,Int) => Int = (a,b) => a + b
(Option(1), Option(2)).mapN(add)
//(Option("cats"), Option(true)).mapN(add)

import cats.Monoid
import cats.instances.boolean._
import cats.instances.int._
import cats.instances.list._
import cats.instances.string._
import cats.syntax.apply._

val tupleToCat:  (String,Int,String) => Cat = Cat.apply
val catToTuple: Cat => (String, Int, String) = cat => (cat.name, cat.born, cat.color)

implicit val catMonoid: Monoid[Cat] = (
  Monoid[String],
  Monoid[Int],
  Monoid[String]
).imapN(tupleToCat)(catToTuple)

import cats.syntax.semigroup._
val garfield = Cat("Garfield", 1987, "Black & orange")
val heathcliff = Cat("Heathcliff", 1987, "Black & yellow")

garfield |+| heathcliff

import cats.Semigroupal
import cats.instances.future._
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

val futurePair = Semigroupal[Future].product(Future("Hello"), Future(123))
Await.result(futurePair, 1.second)

import cats.syntax.apply._
val futureCat = (
  Future("Garfield"),
  Future(1987),
  Future("Black & orange")
).mapN(Cat.apply)

Await.result(futureCat, 1.second)

import cats.Semigroupal
import cats.instances.list._

Semigroupal[List].product(List(1,2), List(3,4))

import cats.instances.either._

type ErrorOr[A] = Either[Vector[String], A]
Semigroupal[ErrorOr].product(
  Left(Vector("Error 1")),
  Left(Vector("Error 2"))
)

import cats.Monad
import cats.syntax.flatMap._
import cats.syntax.functor._
def product[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A,B)] =
  for {
    a <- x
    b <- y
  } yield (a,b)

product(List("a", "b"), List("c"))

import cats.Semigroupal
import cats.data.Validated
import cats.instances.list._

type AllErrorsOr[A] = Validated[List[String], A]
Semigroupal[AllErrorsOr].product(
  Validated.invalid(List("Error 1")),
  Validated.invalid(List("Error 2"))
)

val v = Validated.Valid(123)
val i = Validated.Invalid(List("Badness"))

Validated.valid[List[String], Int](123)
Validated.invalid[List[String], Int](List("Badness"))

import cats.syntax.validated._
123.valid[List[String]]
List("Badness").invalid[Int]

import cats.syntax.applicative._
import cats.syntax.applicativeError._

type ErrorsOr[A] = Validated[List[String], A]

123.pure[ErrorsOr]
List("Badness").raiseError[ErrorsOr, Int]

Validated.catchOnly[NumberFormatException]("foo".toInt)
Validated.catchNonFatal(sys.error("Badness"))
Validated.fromTry(util.Try("foo".toInt))
Validated.fromEither[String,Int](Left("Badness"))
Validated.fromOption[String,Int](None, "Badness")

//type AllErrorsOr[A] = Validated[String, A]

import cats.syntax.apply._
import cats.instances.vector._

(
  Vector(404).invalid[Int],
  Vector(500).invalid[Int]
).tupled

import cats.data.NonEmptyVector

(
  NonEmptyVector.of("Error 1").invalid[Int],
  NonEmptyVector.of("Error 2").invalid[Int]
).tupled

123.valid.map(_ * 100)
"?".invalid.leftMap(_.toString)
123.valid[String].bimap(_ + "!", _ * 100)
"?".invalid[Int].bimap(_ + "!", _ * 100)

import cats.syntax.either._

"Badness".invalid[Int]
"Badness".invalid[Int].toEither
"Badness".invalid[Int].toEither.toValidated

41.valid[String].withEither(_.flatMap(n => Right(n + 1)))

123.valid[String].ensure("Negative!")(_ > 0)

"fail".invalid[Int].getOrElse(0)
"fail".invalid[Int].fold(_ + "!!!", _.toString)


import cats.data.Validated

case class User(name: String, age: Int)

type FormData = Map[String,String]
type FailFast[A] = Either[List[String], A]
type FailSlow[A] = Validated[List[String], A]

def getValue(field: String)(data: Map[String,String]): FailFast[String] =
  data.get(field).toRight(List(s"$field field not specified"))

def getName = getValue("name") _
def getAge = getValue("age") _

getName(Map("name" -> "Dade Murphy"))
getName(Map())

import cats.syntax.either._
def parseInt(field: String)(data: String): FailFast[Int] =
  Either.catchOnly[NumberFormatException](data.toInt)
    .leftMap(_ => List(s"$field must be an integer"))

parseInt("age")("11")
parseInt("age")("foo")

def nonBlank(field: String)(data: String): FailFast[String] =
  Right(data).ensure(List(s"$field cannot be blank"))(_.nonEmpty)

nonBlank("name")("Dade Murphy")
nonBlank("name")("")

def nonNegative(field: String)(data: Int) =
  Right(data).ensure(List(s"$field cannot be negative"))(_ >= 0)

nonNegative("age")(11)
nonNegative("age")(-1)

def readName(data: FormData): FailFast[String] =
  getName(data).flatMap(nonBlank("name"))

def readAge(data: FormData): FailFast[Int] =
  for {
    age <- getAge(data)
    ageNum <- parseInt("age")(age)
    _ <- nonNegative("age")(ageNum)
  } yield ageNum

readName(Map("name" -> "Dade Murphy"))
readName(Map("name" -> ""))
readName(Map())
readAge(Map("age" -> "11"))
readAge(Map("age" -> "-1"))
readAge(Map())

import cats.instances.list._
import cats.syntax.apply._

def readUser(data: FormData): FailSlow[User] =
  (
    readName(data).toValidated,
    readAge(data).toValidated
  ).mapN(User.apply)

readUser(Map("name" -> "Dave", "age" -> "37"))
readUser(Map("age" -> "-1"))