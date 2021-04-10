import java.util.concurrent.FutureTask

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

val hostnames = List(
  "alpha.example.com",
  "beta.example.com",
  "gamma.demo.com"
)

def getUptime(hostname: String): Future[Int] =
  Future(hostname.length * 60)

val allUptimes: Future[List[Int]] =
  hostnames.foldLeft(Future(List.empty[Int])) {
    (acc,host) =>
      val uptime = getUptime(host)
      for {
        acc <- acc
        uptime <- uptime
      } yield acc :+ uptime
  }

Await.result(allUptimes, 1.second)

val allUptimesV2: Future[List[Int]] =
  Future.traverse(hostnames)(getUptime)

Await.result(allUptimesV2, 1.second)

import cats.Applicative
import cats.instances.future._
import cats.syntax.applicative._

List.empty[Int].pure[Future]    // same as Future(List.empty[Int])

import cats.syntax.apply._

def combine(acc: Future[List[Int]], host: String): Future[List[Int]] =
  (acc, getUptime(host)).mapN(_ :+ _)

def listTraverse[F[_]: Applicative, A, B](as: List[A])(f: A => F[B]): F[List[B]] =
  as.foldLeft(List.empty[B].pure[F]) { (acc,a) =>
    (acc, f(a)).mapN(_ :+ _)
  }

def listSequence[F[_]: Applicative, B](bs: List[F[B]]): F[List[B]] =
  listTraverse(bs)(identity)

val totalUptime = listTraverse(hostnames)(getUptime)

Await.result(totalUptime, 1.second)

import cats.instances.vector._

listSequence(List(Vector(1,2), Vector(3,4)))
listSequence(List(Vector(1,2), Vector(3,4), Vector(5,6)))

import cats.instances.option._

def process(inputs: List[Int]) =
  listTraverse(inputs)(n => if (n % 2 == 0) Some(n) else None)

process(List(2,4,6))
process(List(1,2,3))

import cats.data.Validated
import cats.instances.list._

type ErrorsOr[A] = Validated[List[String], A]

def processV2(inputs: List[Int]): ErrorsOr[List[Int]] =
  listTraverse(inputs) { n =>
    if (n % 2 == 0) Validated.valid(n)
    else Validated.invalid(List(s"$n is not even"))
  }

processV2(List(2,4,6))
processV2(List(1,2,3))

import cats.Traverse
import cats.instances.future._
import cats.instances.list._

val totalUptimeV2: Future[List[Int]] =
  Traverse[List].traverse(hostnames)(getUptime)

Await.result(totalUptimeV2, 1.second)

val numbers = List(Future(1), Future(2), Future(3))
val numbers2: Future[List[Int]] = Traverse[List].sequence(numbers)

Await.result(numbers2, 1.second)

import cats.syntax.traverse._
Await.result(hostnames.traverse(getUptime), 1.second)
Await.result(numbers.sequence, 1.second)