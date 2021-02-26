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

object ImperativeAndLazyIO {
  import java.io._

  def linesGt40k(filename: String): IO[Boolean] = IO {
    val src = io.Source.fromFile(filename)
    try {
      var count = 0
      val lines: Iterator[String] = src.getLines()
      while (count <= 40000 && lines.hasNext) {
        lines.next()
        count += 1
      }
      count > 40000
    } finally src.close()
  }
}

object Examples {
  val lines: Stream[String] = sys.error("defined elsewhere")
  val ex1 = lines.zipWithIndex.exists(_._2 + 1 >= 40000)
  val ex2 = lines.filter(!_.trim.isEmpty).zipWithIndex.exists(_._2 + 1 >= 40000)
  val ex3 = lines.take(40000).map(_.head).indexOfSlice("abracadabra".toList)
}

object SimpleStreamTransducers {
  sealed trait Process[I,O] {
    import Process._

    def apply(s: Stream[I]): Stream[O] = this match {
      case Halt() => Stream()
      case Await(recv) => s match {
        case h #:: t => recv(Some(h))(t)
        case xs => recv(None)(xs)
      }
      case Emit(h,t) => h #:: t(s)
    }
  }

  object Process {
    case class Emit[I,O](head: O, tail: Process[I,O] = Halt[I,O]()) extends Process[I,O]
    case class Await[I,O](recv: Option[I] => Process[I,O]) extends Process[I,O]
    case class Halt[I,O]() extends Process[I,O]

    def emit[I,O](head: O, tail: Process[I,O] = Halt[I,O]()): Process[I,O] =
      Emit(head,tail)
  }
}