trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]
  def distribute[A,B](fab: F[(A,B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))
}

val listFunctor: Functor[List] = new Functor[List] {
  override def map[A, B](fa: List[A])(f: A => B): List[B] = fa map f
}

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]
  def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B]

  def map[A,B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))
  def map2[A,B,C](ma: M[A], mb: M[B])(f: (A,B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a,b)))

  def sequence[A](lma: List[M[A]]): M[List[A]] =
    lma.foldRight(unit(List[A]()))((ma, mla) => map2(ma, mla)(_ :: _))
  def traverse[A,B](la: List[A])(f: A => M[B]): M[List[B]] =
    la.foldRight(unit(List[B]()))((a,mlb) => map2(f(a),mlb)(_ :: _))
  def replicateM[A](n: Int, ma: M[A]): M[List[A]] =
    sequence(List.fill(n)(ma))

  def factor[A,B](ma: M[A], mb: M[B]): M[(A,B)] = map2(ma,mb)((_,_))
  def cofactor[A,B](e: Either[M[A],M[B]]): M[Either[A,B]] = e match {
    case Left(ma) => map(ma)(Left(_))
    case Right(mb) => map(mb)(Right(_))
  }

  def compose[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] =
    a => flatMap(f(a))(g)

  def _flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] =
    compose((_:Unit) => ma, f)(())

  def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(ma => ma)

  def filterM[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] =
    ms.foldRight(unit(List[A]()))((x,y) =>
      compose(f, (b: Boolean) => if (b) map2(unit(x), y)(_ :: _) else y)(x))
}

object Monad {
  val optionMonad = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)
    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma.flatMap(f)
  }

  val listMonad = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)
    override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma.flatMap(f)
  }

  val idMonad = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)
    override def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = ma.flatMap(f)
  }

  class StateMonads[S] {
    type StateS[A] = State[S,A]

    val monad = new Monad[StateS] {
      override def unit[A](a: => A): State[S,A] = State(s => (a,s))
      override def flatMap[A, B](st: State[S,A])(f: A => State[S,B]): State[S,B] =
        st.flatMap(f)
    }
  }

  def stateMonad[S] = new Monad[({type lambda[x] = State[S,x]})#lambda] {
    override def unit[A](a: => A): State[S, A] = State(s => (a,s))
    override def flatMap[A, B](ma: State[S, A])(f: A => State[S, B]): State[S, B] =
      ma.flatMap(f)
  }

  def getState[S]: State[S,S] = State(s => (s,s))
  def setState[S](s: S): State[S,Unit] = State(_ => ((),s))

  val F = stateMonad[Int]

 def zipWithIndex[A](as: List[A]): List[(Int,A)] =
   as.foldLeft(F.unit(List[(Int,A)]()))((acc,a) => for {
     xs <- acc
     n <- getState
     _ <- setState(n+1)
   } yield (n,a) :: xs).run(0)._1.reverse

  case class Reader[R,A](run: R => A)
  def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {
    override def unit[A](a: => A): Reader[R, A] = Reader(_ => a)
    override def flatMap[A, B](ma: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
      Reader(r => f(ma.run(r)).run(r))
  }
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

case class State[S,A](run: S => (A,S)) {
  def map[B](f: A => B): State[S,B] =
    State(s => {
      val (a,s1) = run(s)
      (f(a),s1)
    })
  def flatMap[B](f: A => State[S,B]): State[S,B] =
    State(s => {
      val (a,s1) = run(s)
      f(a).run(s1)
    })
}

object Reader {
  def ask[R]: Monad.Reader[R,R] = Monad.Reader(r => r)
}

for {
  a <- Id("Hello, ")
  b <- Id("monad!")
} yield a + b


