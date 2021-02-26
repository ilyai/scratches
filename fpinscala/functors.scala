import java.util.Date

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]
  def distribute[A,B](fab: F[(A,B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))
}

trait Applicative[F[_]] extends Functor[F] {
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] =
    apply(map(fa)(f.curried))(fb)

  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab,fa)(_(_))

  def unit[A](a: => A): F[A]

  def map[A,B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(fa => fa)

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a,fbs) => map2(f(a),fbs)(_ :: _))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def factor[A,B](fa: F[A], fb: F[B]): F[(A,B)] =
    map2(fa,fb)(_ -> _)

  def product[G[_]](G: Applicative[G]): Applicative[({ type f[x] = (F[x], G[x]) })#f] = {
    val self = this
    new Applicative[({
  type f[x] = (F[x], G[x])
})#f] {
      override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))
      override def apply[A, B](fab: (F[A => B], G[A => B]))(fa: (F[A], G[A])): (F[B], G[B]) =
        (self.apply(fab._1)(fa._1), G.apply(fab._2)(fa._2))
    }
  }

  def compose[G[_]](G: Applicative[G]): Applicative[({ type f[x] = F[G[x]] })#f] = {
    val self = this
    new Applicative[({
  type f[x] = F[G[x]]
})#f] {
      override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))
      override def map2[A, B, C](fga: F[G[A]], fgb: F[G[B]])(f: (A, B) => C): F[G[C]] =
        self.map2(fga,fgb)(G.map2(_,_)(f))
    }
  }

  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] =
    ofa.foldLeft(unit(Map.empty[K,V])) {
      case (acc, (k,fv)) => map2(acc,fv)((m,v) => m + (k -> v))
    }
}

sealed trait Validation[+E, +A]
case class Failure[E](head: E, tail: Vector[E]) extends Validation[E, Nothing]
case class Success[A](a: A) extends Validation[Nothing, A]

object Applicative {
  val streamApplicative = new Applicative[Stream] {
    override def unit[A](a: => A): Stream[A] = Stream.continually(a)
    override def map2[A, B, C](fa: Stream[A], fb: Stream[B])(f: (A, B) => C): Stream[C] =
      fa zip fb map f.tupled
  }

  def validationApplicative[E]: Applicative[({ type f[x] = Validation[E,x] })#f] =
    new Applicative[({
  type f[x] = Validation[E, x]
})#f] {
      override def unit[A](a: => A): Validation[E, A] = Success(a)
      override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] =
        (fa,fb) match {
          case (Success(a), Success(b)) => Success(f(a,b))
          case (Failure(h1,t1), Failure(h2,t2)) => Failure(h1, t1 ++ Vector(h2) ++ t2)
          case (e @ Failure(_,_), _) => e
          case (_, e @ Failure(_,_)) => e
        }
    }

  type Const[A,B] = A

  trait Monoid[A] {
    def op(a1: A, a2: A): A
    def zero: A
  }

  implicit def monoidApplicative[M](M: Monoid[M])  =
    new Applicative[({ type f[x] = Const[M, x] })#f] {
      override def unit[A](a: => A): Const[M, A] = M.zero
      override def apply[A, B](fab: Const[M, A => B])(fa: Const[M, A]): Const[M, B] = M.op(fab,fa)
    }
}

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B] =
    join(map(ma)(f))

  override def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    flatMap(fab)(f => map(fa)(f))

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))

  override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a,b)))

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)
}

object Monad {
  def eitherMonad[E]: Monad[({ type f[x] = Either[E,x] })#f] =
    new Monad[({
  type f[x] = Either[E, x]
})#f] {
      override def unit[A](a: => A): Either[E, A] = Right(a)
      override def flatMap[A, B](ma: Either[E, A])(f: A => Either[E, B]): Either[E, B] =
        ma match {
          case Right(a) => f(a)
          case Left(b) => Left(b)
        }
    }

  def composeM[G[_],H[_]](implicit G: Monad[G], H: Monad[H], T: Traverse[H]):
    Monad[({ type f[x] = G[H[x]] })#f] = new Monad[({
  type f[x] = G[H[x]]
})#f] {
    override def unit[A](a: => A): G[H[A]] = G.unit(H.unit(a))
    override def flatMap[A, B](ma: G[H[A]])(f: A => G[H[B]]): G[H[B]] =
      G.flatMap(ma)(a => G.map(T.traverse(a)(f))(H.join))
  }
}

trait Traverse[F[_]] extends Functor[F] { self =>
  def traverse[M[_]: Applicative,A,B](fa: F[A])(f: A => M[B]): M[F[B]] =
    sequence(map(fa)(f))

  def sequence[M[_]: Applicative,A](fma: F[M[A]]): M[F[A]] =
    traverse(fma)(ma => ma)

  type Id[A] = A

  val idMonad = new Monad[Id] {
    override def unit[A](a: => A) = a
    override def flatMap[A, B](a: A)(f: A => B): B = f(a)
  }

  def map[A,B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)(idMonad)

  import Applicative._

  def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]) =
    traverse[({ type f[x] = Const[B,x] })#f,A,Nothing](as)(f)(monoidApplicative(mb))
}

case class Tree[+A](head: A, tail: List[Tree[A]])

case class WebForm(name: String, birthday: Date, phone: String)

def validName(name: String): Validation[String,String] =
  if (name != "") Success(name)
  else Failure("Name cannot be empty", Vector.empty)

def validPhone(phone: String): Validation[String,String] =
  if (phone.matches("[0-9]{10}")) Success(phone)
  else Failure("Phone number must be 10 digits", Vector.empty)

def validBirthday(birthday: String): Validation[String,Date] =
  try {
    import java.text._
    Success(new SimpleDateFormat("yyyy-MM-dd").parse(birthday))
  } catch { case _: Exception =>
    Failure("Birthday must be in the form yyyy-MM-dd", Vector.empty)
  }

val V = Applicative.validationApplicative[String]

def validWebForm(name: String, birthday: String, phone: String): Validation[String, WebForm] =
  V.apply(V.apply(V.apply(V.unit((WebForm(_,_,_)).curried))(
    validName(name)))(
    validBirthday(birthday)))(
    validPhone(phone))

validWebForm("John", "1950-12-12", "0123456789")

val f: (Int,Int,Int) => Int = (_ + _ + _)
val g: Int => Int => Int => Int = f.curried

f(1,2,3)
g(1)(2)(3)
