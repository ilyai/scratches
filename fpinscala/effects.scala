import scala.io.StdIn.readLine
import language.postfixOps
import language.higherKinds

case class Player(name: String, score: Int)

object IO1 {

  def printWinner(p: Player) = println(s"${p.name} is the winner")
  def winner(p1: Player, p2: Player) = if (p1.score > p2.score) p1 else p2
  def declareWinner(p1: Player, p2: Player) = printWinner(winner(p1,p2))

  trait IO[+A] { self =>
    def run: A
    def map[B](f: A => B): IO[B] = new IO[B] {
      override def run: B = f(self.run)
    }
    def flatMap[B](f: A => IO[B]): IO[B] = new IO[B] {
      override def run: B = f(self.run).run
    }
  }

  object IO {
    def unit[A](a: => A): IO[A] = new IO[A] {
      override def run: A = a
    }
    def flatMap[A,B](fa: IO[A])(f: A => IO[B]) = fa.flatMap(f)
    def apply[A](a: => A): IO[A] = unit(a)
    def empty: IO[Unit] = new IO[Unit] {
      override def run: Unit = ()
    }
  }

  def fahrenheitToCelsius(f: Double): Double =
    (f-32) * 5.0/9.0

  def readLine: IO[String] = IO { scala.io.StdIn.readLine }
  def printLine(msg: String): IO[Unit] = IO { println(msg) }

  def converter: IO[Unit] = for {
    _ <- printLine("Enter a temperature in degrees fahrenheit: ")
    d <- readLine.map(_.toDouble)
    _ <- printLine(fahrenheitToCelsius(d).toString)
  } yield ()

  val echo = readLine.flatMap(printLine)
  val readInt = readLine.map(_.toInt)

}


object IO2a {

  sealed trait IO[A] {
    def flatMap[B](f: A => IO[B]): IO[B] =
      FlatMap(this, f)
    def map[B](f: A => B): IO[B] =
      flatMap(f andThen (Return(_)))
  }

  case class Return[A](a: A) extends IO[A]
  case class Suspend[A](resume: () => A) extends IO[A]
  case class FlatMap[A,B](sub: IO[A], k: A => IO[B]) extends IO[B]

  def printLine(s: String): IO[Unit] =
    Suspend(() => Return(println(s)))

//  def run[A](io: IO[A]): A = io match {
//    case Return(a) => a
//    case Suspend(r) => r()
//    case FlatMap(x,f) => x match {
//      case Return(a) => run(f(a))
//      case Suspend(r) => run(f(r()))
//      case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
//    }
//  }

}