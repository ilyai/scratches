def parseInt(str: String): Option[Int] =
  scala.util.Try(str.toInt).toOption

def divide(a: Int, b: Int): Option[Int] =
  if (b == 0) None else Some(a / b)

//def stringDivideBy(aStr: String, bStr: String): Option[Int] =
//  parseInt(aStr).flatMap { aNum =>
//    parseInt(bStr).flatMap { bNum =>
//      divide(aNum, bNum)
//    }
//  }

def stringDivideBy(aStr: String, bStr: String): Option[Int] =
  for {
    aNum <- parseInt(aStr)
    bNum <- parseInt(bStr)
    ans <- divide(aNum, bNum)
  } yield ans

stringDivideBy("6", "2")
stringDivideBy("6", "0")
stringDivideBy("6", "foo")
stringDivideBy("bar", "2")

for {
  x <- (1 to 3).toList
  y <- (4 to 5).toList
} yield (x,y)

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

def doSomethingLongRunning: Future[Int] = ???
def doSomethingElseLongRunning: Future[Int] = ???

def doSomethingVeryLongRunning: Future[Int] =
  for {
    result1 <- doSomethingLongRunning
    result2 <- doSomethingElseLongRunning
  } yield result1 + result2

trait Monad[F[_]] {
  def pure[A](value: A): F[A]
  def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]
  def map[A,B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => pure(f(a)))
}

import cats.instances.option._
import cats.instances.list._
import cats.instances.vector._
val opt1 = cats.Monad[Option].pure(3)
val opt2 = cats.Monad[Option].flatMap(opt1)(a => Some(a + 2))
val opt3 = cats.Monad[Option].map(opt2)(100 * _)

val list1 = cats.Monad[List].pure(3)
val list2 = cats.Monad[List].flatMap(List(1,2,3))(a => List(a, a*10))
val list3 = cats.Monad[List].map(list2)(_ + 123)
val vector1 = cats.Monad[Vector].flatMap(Vector(1,2,3))(a => Vector(a, a*10))

import cats.instances.future._
import scala.concurrent._
import scala.concurrent.duration._
val future = cats.Monad[Future].flatMap(cats.Monad[Future].pure(1))(x =>
  cats.Monad[Future].pure(x + 2)
)
Await.result(future, 1.second)

import cats.syntax.applicative._
1.pure[Option]
1.pure[List]

import cats.syntax.functor._
import cats.syntax.flatMap._

def sumSquare[F[_]: cats.Monad](a: F[Int], b: F[Int]): F[Int] =
//  a.flatMap(x => b.map(y => x*x + y*y))
  for {
    x <- a
    y <- b
  } yield x*x + y*y

sumSquare(Option(3), Option(4))
sumSquare(List(1,2,3), List(4,5))
sumSquare(3: cats.Id[Int],4: cats.Id[Int])

val a = cats.Monad[cats.Id].pure(3)
val b = cats.Monad[cats.Id].flatMap(a)(_ + 1)

for (x <- a; y <- b) yield x+y

def pure[A](value: A): cats.Id[A] = value
def map[A,B](value: A)(f: A => B): cats.Id[B] = f(value)
def flatMap[A,B](value: A)(f: A => cats.Id[B]): cats.Id[B] =
  f(value)

val either1: Either[String, Int] = Right(10)
val either2: Either[String, Int] = Right(32)

for {
  a <- either1
  b <- either2
} yield a + b

import cats.syntax.either._
val aa = 3.asRight[String]
val bb = 4.asRight[String]

for {
  x <- aa
  y <- bb
} yield x*x + y*y

def countPositive(nums: List[Int]) =
//  nums.foldLeft(Right(0): Either[String,Int]) { (acc, num) =>
  nums.foldLeft(0.asRight[String]) { (acc, num) =>
    if (num > 0) acc.map(_ + 1) else Left("Negative. Stopping!")
  }

countPositive(List(1,2,3))
countPositive(List(1,-2,3))

Either.catchOnly[NumberFormatException]("foo".toInt)
Either.catchNonFatal(sys.error("Badness"))

Either.fromTry(scala.util.Try("foo".toInt))
Either.fromOption(None, "Badness")

"Error".asLeft[Int].getOrElse(0)
"Error".asLeft[Int].orElse(2.asRight[String])
(-1).asRight[String].ensure("Must be non-negative")(_ > 0)
"error".asLeft[Int].recover {
  case _: String => -1
}
"error".asLeft[Int].recoverWith {
  case _: String => Right(-1)
}
"foo".asLeft[Int].leftMap(_.reverse)
6.asRight[String].bimap(_.reverse, _*7)
"bar".asLeft[Int].bimap(_.reverse, _*7)
123.asRight[String]
123.asRight[String].swap

for {
  a <- 1.asRight[String]
  b <- 0.asRight[String]
  c <- if (b == 0) "Division by zero".asLeft[Int]
  else (a/b).asRight[String]
} yield c * 100

type Result[A] = Either[Throwable, A]

sealed trait LoginError extends Product with Serializable
final case class UserNotFound(username: String) extends LoginError
final case class PasswordIncorrect(username: String) extends LoginError
case object UnexpectedError extends LoginError

case class User(username: String, password: String)
type LoginResult = Either[LoginError, User]

def handleError(error: LoginError): Unit =
  error match {
    case UserNotFound(u) => println(s"User not found: $u")
    case PasswordIncorrect(u) => println(s"Password incorrect: $u")
    case UnexpectedError => println(s"Unexpected error")
  }

val result1: LoginResult = User("dave", "passw0rd").asRight
val result2: LoginResult = UserNotFound("dave").asLeft

result1.fold(handleError, println)
result2.fold(handleError, println)

trait MonadError[F[_], E] extends cats.Monad[F] {
  def raiseError[A](e: E): F[A]
  def handleError[A](fa: F[A])(f: E => A): F[A]
  def ensure[A](fa: F[A])(e: E)(f: A => Boolean): F[A]
}

//import cats.MonadError
//import cats.instances.either._
type ErrorOr[A] = Either[String, A]
//val monadError = cats.MonadError[ErrorOr, String]

//import cats.syntax.applicative._
import cats.syntax.applicativeError._
//import cats.syntax.monadError._
//val success = 42.pure[ErrorOr]

import cats.instances.try_._
val ex: Throwable = new RuntimeException("It's all gone wrong")
ex.raiseError[scala.util.Try, Int]

val x = {
  println("computing x")
  math.random
}

x
x

def y = {
  println("computing y")
  math.random
}

y
y

lazy val z = {
  println("computing z")
  math.random
}

z
z

import cats.Eval
val now = Eval.now(math.random()+1000)    // val
val always = Eval.always(math.random()+3000)    // def
val later = Eval.later(math.random()+2000)    // lazy val


now.value
always.value
later.value

val greeting = Eval
  .always { println("Step 1"); "hello" }
  .map { str => println("Step 2"); s"$str world" }
  .memoize
  .map { str => println("Step 3"); s"$str!" }

greeting.value
greeting.value

val ans = for {
  a <- Eval.now { println("Calculating A"); 40 }
  b <- Eval.always { println("Calculating B"); 2 }
} yield {
  println("Adding A and B")
  a + b
}

ans.value
ans.value

def factorial(n: BigInt): Eval[BigInt] =
  if (n == 1) Eval.now(n)
  else Eval.defer(factorial(n - 1).map(_ * n))

//factorial(50000).value

def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): Eval[B] =
  as match {
    case h :: t => Eval.defer(foldRight(t,z)(f).map(f(h, _)))
    case Nil => Eval.now(z)
  }

//foldRight((1 to 50000).toList, 0)(_ + _).value
(((50000:BigInt)+1)*50000)/2

import cats.data.Writer
import cats.instances.vector._
Writer(Vector(
  "It was the best of times",
  "it was the worst of times"
), 1859)

import cats.syntax.applicative._
type Logged[A] = Writer[Vector[String], A]
123.pure[Logged]

import cats.syntax.writer._
Vector("msg1", "msg2", "msg3").tell
val aaa = Writer(Vector("msg1", "msg2", "msg3"), 123)
val bbb = 123.writer(Vector("msg1", "msg2", "msg3"))

aaa.value
aaa.written
val (log, result) = bbb.run

val writer1 = for {
  a <- 10.pure[Logged]
  _ <- Vector("a","b","c").tell
  b <- 32.writer(Vector("x","y","z"))
} yield a + b

val writer2 = writer1.mapWritten(_.map(_.toUpperCase))
val writer3 = writer1.bimap(_.map(_.toUpperCase), _*100)
val writer4 = writer1.mapBoth((log,res) => (log.map(_.toUpperCase), res*100))
writer2.run
writer3.run
writer4.run

writer1.reset.run
writer1.swap.run

def slowly[A](body: => A): A =
  try body finally Thread.sleep(100)

//def factorial(n: Int): Int = {
//  val ans = slowly(if (n == 0) 1 else n*factorial(n-1))
//  println(s"fact $n $ans")
//  ans
//}

import cats.syntax.applicative._
import cats.syntax.writer._
import cats.instances.list._
type ListLogged[A] = Writer[List[String], A]
def factorial(n: Int): ListLogged[Int] = {
  for {
    ans <- if (n == 0) 1.pure[ListLogged]
      else slowly(factorial(n-1).map(_ * n))
    _ <- List(s"fact $n $ans").tell
  } yield ans
}

factorial(5)

val Vector((log1,ans1), (log2,ans2)) = Await.result(Future.sequence(Vector(
  Future(factorial(3)),
  Future(factorial(5))
)), 5.seconds).map(_.run)

log1
ans1
log2
ans2

import cats.data.Reader
case class Cat(name: String, food: String)
val catName: Reader[Cat, String] =
  Reader(cat => cat.name)

catName.run(Cat("Garfield", "lasagne"))

val greetKitty: Reader[Cat, String] =
  catName.map(name => s"Hello $name")

greetKitty.run(Cat("Healthcliff", "junk food"))

val feedKitty: Reader[Cat,String] =
  Reader(cat => s"Have a nice bowl of ${cat.food}")

val greetAndFeed: Reader[Cat,String] =
  for {
    greet <- greetKitty
    feed <- feedKitty
  } yield s"$greet. $feed"

greetAndFeed(Cat("Garfield", "lasagne"))
greetAndFeed(Cat("Healthcliff", "junk food"))

case class Db(usernames: Map[Int,String], passwords: Map[String,String])
import cats.syntax.applicative._
//import cats._
type DbReader[A] = Reader[Db, A]
def findUsername(userId: Int): DbReader[Option[String]] =
  Reader(db => db.usernames.get(userId))
def checkPassword(username: String, password: String): DbReader[Boolean] =
  Reader(db => db.passwords.get(username).contains(password))
def checkLogin(userId: Int, password: String): DbReader[Boolean] =
  for {
    maybeUsername <- findUsername(userId)
    isPasswordOk <- maybeUsername.map(checkPassword(_, password)).getOrElse(false.pure[DbReader])
  } yield isPasswordOk

val users = Map(
  1 -> "dade",
  2 -> "kate",
  3 -> "margo"
)

val passwords = Map(
  "dade" -> "zerocool",
  "kate" -> "acidburn",
  "margo" -> "secret"
)
val db = Db(users, passwords)
checkLogin(1, "zerocool").run(db)
checkLogin(4, "davinci").run(db)

import cats.data.State
val aaaa = State[Int,String] { state =>
  (state, s"The state is $state")
}

val (state, resultA) = aaaa.run(10).value
aaaa.runS(10).value
aaaa.runA(10).value

val step1 = State[Int,String] { num =>
  val ans = num + 1
  (ans, s"Result of step1: $ans")
}

val step2 = State[Int,String] { num =>
  val ans = num * 2
  (ans, s"Result of step2: $ans")
}

val both = for {
  a <- step1
  b <- step2
} yield (a,b)

val (state2, resultA2) = both.run(20).value

val getDemo = State.get[Int]
getDemo.run(10).value

val setDemo = State.set[Int](30)
setDemo.run(10).value

val pureDemo = State.pure[Int,String]("Result")
pureDemo.run(10).value

val inspectDemo = State.inspect[Int,String](_ + "!")
inspectDemo.run(10).value

val modifyDemo = State.modify[Int](_ + 1)
modifyDemo.run(10).value

import State._
val program: State[Int, (Int,Int,Int)] = for {
  a <- get[Int]
  _ <- set[Int](a + 1)
  b <- get[Int]
  _ <- modify[Int](_ + 1)
  c <- inspect[Int, Int](_ * 1000)
} yield (a,b,c)

val (state3,result3) = program.run(1).value

import cats.data.State
type CalcState[A] = State[List[Int], A]

def evalOne(sym: String): CalcState[Int] = {
  def operator(f: (Int,Int) => Int): CalcState[Int] =
    State[List[Int],Int] {
      case a :: b :: tail =>
        val ans = f(a,b)
        (ans :: tail, ans)
      case _ => sys.error("Fail!")
    }
  def operand(num: Int): CalcState[Int] =
    State { stack =>
      (num :: stack, num)
    }
  sym match {
    case "+" => operator(_ + _)
    case "*" => operator(_ * _)
    case num => operand(num.toInt)
  }
}

evalOne("42").runA(Nil).value

(for {
  _ <- evalOne("1")
  _ <- evalOne("2")
  ans <- evalOne("+")
} yield ans).runA(Nil).value

def evalAll(input: List[String]): CalcState[Int] =
  input.foldLeft(0.pure[CalcState])((a,b) => a.flatMap(_ => evalOne(b)))

evalAll(List("1","2","+","3","*")).runA(Nil).value

(for {
  _ <- evalAll(List("1","2","+"))
  _ <- evalAll(List("3","4","+"))
  ans <- evalOne("*")
} yield ans).runA(Nil).value

def evalInput(input: String): Int =
  evalAll(input.split(" ").toList).runA(Nil).value

evalInput("1 2 + 3 *")

import cats.Monad
import scala.annotation.tailrec

val optionMonad = new cats.Monad[Option] {
  override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
  override def pure[A](value: A): Option[A] = Some(value)
  override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] =
    f(a) match {
      case None => None
      case Some(Left(value)) => tailRecM(value)(f)
      case Some(Right(value)) => Some(value)
    }
}

sealed trait Tree[+A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]

def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left,right)
def leaf[A](value: A): Tree[A] = Leaf(value)

implicit val treeMonad = new cats.Monad[Tree] {
  override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = {
    fa match {
      case Branch(l,r) => Branch(flatMap(l)(f), flatMap(r)(f))
      case Leaf(v) => f(v)
    }
  }
  override def pure[A](value: A): Tree[A] = Leaf(value)
  override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] =
    f(a) match {
      case Branch(l,r) =>
        Branch(
          flatMap(l) {
            case Left(l) => tailRecM(l)(f)
            case Right(l) => pure(l)
          },
          flatMap(r) {
            case Left(r) => tailRecM(r)(f)
            case Right(r) => pure(r)
          }
        )
      case Leaf(Left(value)) => tailRecM(value)(f)
      case Leaf(Right(value)) => Leaf(value)
    }
}

branch(leaf(100), leaf(200)).flatMap(
  x => branch(leaf(x-1), leaf(x+1))
)

for {
  a <- branch(leaf(100), leaf(200))
  b <- branch(leaf(a - 10), leaf(a + 10))
  c <- branch(leaf(b - 1), leaf(b + 1))
} yield c