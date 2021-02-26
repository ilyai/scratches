import Stream.{empty, unfold}

import scala.annotation.tailrec

(1 to 4).toList map(_ + 10) filter(_ % 2 == 0) map(_ * 3)

true || { println("!!"); false }

def if2[A](cond: Boolean, onTrue: => A, onFalse: => A): A =
  if (cond) onTrue else onFalse

if2 (false, sys.error("fail"), 3 )

def pair(i: => Int) = (i, i)

pair { println("hi"); 1 + 41 }

def pair2(i: => Int) = { lazy val j = i; (j,j) }

pair2 { println("hi"); 1 + 41 }

trait Stream[+A] {
  def toList: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    @tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h,t) =>
        buf += h()
        go(t())
      case _ => buf.toList
    }
    go(this)
  }
  def take(n: Int): Stream[A] = this match {
    case Cons(h,t) if n > 1 => Stream.cons(h(), t().take(n-1))
    case Cons(h,_) if n == 1 => Stream.cons(h(), empty)
    case _ => empty
  }
  def takeViaUnfold(n: Int): Stream[A] = unfold(this) {
    case Cons(h,t) if n > 1 => Some(h() -> t().take(n-1))
    case Cons(h,_) if n == 1 => Some(h(), empty)
    case _ => None
  }
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if p(h()) => Stream.cons(h(), t() takeWhile p)
    case _ => empty
  }
  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h,t) if p(h()) => Some(h(), t() takeWhile p)
    case _ => None
  }
  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a,b) => if (p(a)) Stream.cons(a,b) else b)
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }
  def exists(p: A => Boolean): Boolean = foldRight(false)((a,b) => p(a) || b)
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a,b) => p(a) && b)
  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a,b) => Stream.cons(f(a), b))
  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h,t) => Some(f(h()) -> t())
    case _ => None
  }
  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((a,b) => if (p(a)) Stream.cons(a,b) else b)
  def append[AA >: A](as: Stream[AA]): Stream[AA] = foldRight(as)((a,b) => Stream.cons(a,b))
  def flatMap[AA >: A](f: A => Stream[AA]): Stream[AA] = foldRight(empty[AA])((a,b) => f(a).append(b))
  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] = unfold(this, s2) {
    case (Cons(h1,t1),Cons(h2,t2)) => Some(f(h1(),h2()) -> (t1(),t2()))
    case _ => None
  }
  def zip[B](sb: Stream[B]): Stream[(A,B)] = zipWith(sb)(_ -> _)
  def zipWithAll[B,C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    unfold((this, s2)) {
      case (Empty,Empty) => None
      case (Cons(h,t), Empty) => Some(f(Some(h()),None) -> (t(), empty))
      case (Empty,Cons(h,t)) => Some(f(None,Some(h())) -> (empty,t()))
      case (Cons(h1,t1),Cons(h2,t2)) => Some(f(Some(h1()),Some(h2())) -> (t1(),t2()))
    }
  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    zipWithAll(s2)(_ -> _)
  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(_._2.isDefined) forAll {
      case (h,h2) => h == h2
    }
  def drop(n: Int): Stream[A] = this match {
    case Cons(_,t) if n > 0 => t().drop(n-1)
    case _ => this
  }
  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some(s, s drop 1)
    } append(empty)
  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists(_ startsWith s)
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight(z, Stream(z))((a,p0) => {
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, Stream.cons(b2,p1._2))
    })._2
}

object Stream {
  def empty[A]: Stream[A] = Empty
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  def constant[A](a: A): Stream[A] = cons(a, constant(a))
  def constant2[A](a: A): Stream[A] = unfold(a)(a => Some((a,a)))
  def from(n: Int): Stream[Int] = cons(n, from(n+1))
  def from2(n: Int): Stream[Int] = unfold(n)(n => Some((n,n+1)))
  def fibs: Stream[Int] = {
    def go(p: Int, n: Int): Stream[Int] = cons(p, go(n, p+n))
    go(0,1)
  }
  def fibs2: Stream[Int] = unfold((0,1))(p => Some(p._1 -> (p._2,p._1+p._2)))
  def unfold[A,S](z: S)(f: S => Option[(A,S)]): Stream[A] = f(z) match {
    case None => empty[A]
    case Some((a,s)) => cons(a, unfold(s)(f))
  }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]



val s = Stream(1,2,3)
s.toList
s.take(2).toList
s.takeWhile(_ < 3).toList
s.takeWhile2(_ < 3).toList
s.exists(_ == 2)
s.forAll(_ > 0)
s.forAll(_ < 3)
s.map(_ * 2).toList
s.mapViaUnfold(_ * 2).toList
s.filter(_ < 3).toList
s.append(Stream(4,5,6)).toList
s.flatMap(as => Stream(as,as)).toList

Stream((1 to 4): _*) map(_ + 10) filter(_ % 2 == 0) map(_ * 3) toList

//val ones: Stream[Int] = Stream.cons(1, ones)
//val ones = constant(1)
val ones = Stream.constant2(1)
ones.take(5).toList
ones.exists(_ % 2 != 0)
ones.map(_ + 1).exists(_ % 2 == 0)
ones.takeWhile(_ > 1)
ones.forAll(_ != 1)

Stream.from(1).take(5).toList
Stream.from2(1).take(5).toList
Stream.fibs.take(10).toList
Stream.fibs2.take(10).toList

Stream(1,2,3).scanRight(0)(_ + _).toList