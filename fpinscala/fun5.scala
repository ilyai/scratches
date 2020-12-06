List(1,2,3,4).map(_ + 10).filter(_ % 2 == 0).map(_ * 3)
List(11,12,13,14).filter(_ % 2 == 0).map(_ * 3)
List(12,14).map(_ * 3)
List(36,42)

def square(x: Double): Double = x * x


val x = (true || sys.error("bar"))

square(41.0 + 1.0)
//square(sys.error("failure"))

def if2[A](cond: Boolean, onTrue: => A, onFalse: => A): A = if (cond) onTrue else onFalse

if2(false, sys.error("fail"), 3)

def pair(i: => Int) = (i, i)
pair { print("hi"); 1 + 41 }

def pair2(i: => Int) = { lazy val j = i; (j, j) }
pair2 { println("hi"); 1 + 41 }

def maybeTwice2(b: Boolean, i: => Int) = {
  lazy val j = i
  if (b) j+j else 0
}


val y = maybeTwice2(true, { println("hi"); 1+41 })
val z = maybeTwice2(true, { println("hi"); 1+41 })


sealed trait Stream[+A] {

  import Stream._

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] = this match {
    case _ if n == 0 => Empty
    case Cons(h, t) => cons(h(), t().take(n-1))
    case _ => Empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_,t) if n > 0 => t().drop(n-1)
    case _ => this
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h,t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a,b) => p(a) && b)

  def takeWhile_1(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a,b) => if (p(a)) cons(a, b) else empty[A])

  def headOption_1: Option[A] =
    foldRight(None:Option[A])((a,_) => Some(a))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a,b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a,b) => if (p(a)) cons(a,b) else b)

  def append[B >: A](bs: => Stream[B]): Stream[B] =
    foldRight(bs)((a,b) => cons(a,b))

  def flatMap[B >: A](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a,b) => f(a).append(b))

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption

  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h,t) => Some((f(h()), t()))
    case _ => None
  }

  def takeViaUnfold(n: Int): Stream[A] = unfold((this,n)) {
    case (Cons(h,t),1) => Some(h(), (empty[A], 0))
    case (Cons(h,t),n) if n > 1 => Some((h(), (t(), n - 1)))
    case _ => None
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this) {
    case (Cons(h,t)) if p(h()) => Some((h(), t()))
    case _ => None
  }

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] = unfold((this,s2)) {
    case (Cons(h1,t1), Cons(h2,t2)) => Some(f(h1(),h2()), (t1(),t2()))
    case _ => None
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = zipWithAll(s2)((_, _))

  def zipWithAll[B,C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] = unfold((this,s2)) {
    case (Cons(h1,t1), Cons(h2,t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    case (Cons(h,t), Empty) => Some(f(Some(h()),Option.empty[B]) -> (t(), empty[B]))
    case (Empty, Cons(h,t)) => Some(f(Option.empty[A],Some(h())) -> (empty[A], t()))
    case (Empty, Empty) => None
  }

  def startsWith[A](s: Stream[A]): Boolean = zipAll(s).takeWhile(!_._2.isEmpty) forAll {
    case (a,b) => a == b
  }

  def tails: Stream[Stream[A]] = unfold(this) {
    case Empty => None
    case Cons(h,t) => Some(Cons(h, t) -> t())
  } append Stream(empty)

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a,p0) => {
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  val fibs = {
    def go(f0: Int, f1: Int): Stream[Int] = {
      cons(f0, go(f1, f0+f1))
    }
    go(0, 1)
  }

  def unfold[A,S](z: S)(f: S => Option[(A,S)]): Stream[A] = f(z) match {
    case None => empty
    case Some((h,t)) => cons(h, unfold(t)(f))
  }

  def fibsViaUnfold = {
    unfold((0,1)) {
      case (f0,f1) => Some((f0, (f1, f0+f1)))
    }
  }

  def fromViaUnfold(n: Int) = unfold(n)(n => Some(n,n+1))

  def constantViaUnfold[A](a: A) = unfold(a)(_ => Some(a, a))

}

Stream(1,2,3).toList
Stream(1,2,3).take(2).toList
Stream(1,2,3).takeWhile(_ < 2).toList
Stream(1,2,3).takeWhileViaUnfold(_ < 3).toList
Stream(1,2,3).drop(2).toList

Stream(1,2,3,4).map(_ + 10).filter(_ % 2 == 0).toList

val ones: Stream[Int] = Stream.cons(1, ones)
val onesViaUnfold: Stream[Int] = Stream.unfold(1)(_ => Some(1,1))

ones.take(5).toList
ones.exists(_ % 2 != 0)
ones.map(_ + 1).exists(_ % 2 == 0)
ones.takeWhile(_ == 1)
ones.forAll(_ != 1)

onesViaUnfold.take(5).toList

Stream.fibs.take(5).toList
Stream.fibsViaUnfold.take(5).toList

Stream(1,2,3,4).startsWith(Stream(1,2,3))
Stream(1,2,3,4).startsWith(Stream(1,3,3))
Stream(1,2,3).tails.flatMap(x => x).toList

Stream(1,2,3).scanRight(0)(_ + _).toList