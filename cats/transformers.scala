import cats.Monad
import cats.syntax.applicative._
import cats.syntax.flatMap._

//def compose[M1[_]: Monad, M2[_]: Monad] = {
//  type Composed[A] = M1[M2[A]]
//
//  new Monad[Composed] {
//    override def pure[A](x: A): Composed[A] =
//      x.pure[M2].pure[M1]
//    override def flatMap[A, B](fa: Composed[A])(f: A => Composed[B]): Composed[B] = ???
//  }
//}

import cats.data.OptionT
type ListOption[A] = OptionT[List,A]

import cats.Monad
import cats.instances.list._
import cats.syntax.applicative._

val result1: ListOption[Int] = OptionT(List(Option(10)))
val result2: ListOption[Int] = 32.pure[ListOption]

result1.flatMap(x => result2.map(y => x+y))

type ErrorOr[A] = Either[String,A]
type ErrorOrOption[A] = OptionT[ErrorOr, A]

import cats.instances.either._
val a = 10.pure[ErrorOrOption]
val b = 32.pure[ErrorOrOption]
val c = a.flatMap(x => b.map(y => x+y))

import scala.concurrent.Future
import cats.data.{EitherT, OptionT}

type FutureEither[A] = EitherT[Future, String, A]
type FutureEitherOption[A] = OptionT[FutureEither, A]

import cats.instances.future._
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

val futureEitherOr: FutureEitherOption[Int] =
  for {
    a <- 10.pure[FutureEitherOption]
    b <- 32.pure[FutureEitherOption]
  } yield a + b

val errorStack1 = OptionT[ErrorOr, Int](Right(Some(10)))
val errorStack2 = 32.pure[ErrorOrOption]

errorStack1.value
errorStack2.value.map(_.getOrElse(-1))

futureEitherOr
val intermediate = futureEitherOr.value
val stack = intermediate.value
Await.result(stack, 1.second)

sealed abstract class HttpError
final case class NotFound(item: String) extends HttpError
final case class BadRequest(msg: String) extends HttpError

//type FutureEither[A] = EitherT[Future, HttpError, A]

import cats.data.Writer
type Logged[A] = Writer[List[String], A]

def parseNumber(str: String): Logged[Option[Int]] =
  util.Try(str.toInt).toOption match {
    case Some(num) => Writer(List(s"Read $str"), Some(num))
    case None => Writer(List(s"Failed on $str"), None)
  }

def addAll(a: String, b: String, c: String): Logged[Option[Int]] = {
  import cats.data.OptionT
  val result = for {
    a <- OptionT(parseNumber(a))
    b <- OptionT(parseNumber(b))
    c <- OptionT(parseNumber(c))
  } yield a + b + c
  result.value
}

addAll("1", "2", "3")
addAll("1", "a", "3")

//type Response[A] = Future[Either[String, A]]
type Response[A] = EitherT[Future, String, A]

val powerLevels = Map(
  "Jazz" -> 6,
  "Bumblebee" -> 8,
  "Hot Rod" -> 10
)
def getPowerLevel(autobot: String): Response[Int] =
  powerLevels.get(autobot) match {
    case Some(level) => level.pure[Response]
    case None => EitherT.left(Future(s"no such autobot: $autobot"))
  }

def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
  for {
    l1 <- getPowerLevel(ally1)
    l2 <- getPowerLevel(ally2)
  } yield l1 + l2 > 15

def tacticalReport(ally1: String, ally2: String): String = {
  val stack = canSpecialMove(ally1, ally2).value
  Await.result(stack, 1.second) match {
    case Left(msg) => s"Communication error: $msg"
    case Right(true) => s"$ally1 and $ally2 are ready to roll out!"
    case Right(false) =>  s"$ally1 and $ally2 need a recharge"
  }
}

tacticalReport("Jazz", "Bumblebee")
tacticalReport("Bumblebee", "Hot Rod")
tacticalReport("Jazz", "Ironhide")