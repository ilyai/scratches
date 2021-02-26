import Stream.{constant, empty, unfold}

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
  def uncons: Option[(A, Stream[A])]
  def isEmpty: Boolean = uncons.isEmpty
  def toList: List[A] = uncons.map(t => t._1 :: t._2.toList).getOrElse(Nil)
  def take(n: Int): Stream[A] =
    if (n <= 0) Stream.empty[A]
    else uncons.map(t => Stream.cons(t._1,t._2.take(n-1))).getOrElse(empty)
  def takeWhile(p: A => Boolean): Stream[A] =
    uncons.map(t => if (p(t._1)) Stream.cons(t._1,t._2.takeWhile(p)) else empty).getOrElse(empty)
  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a,b) => if (p(a)) Stream.cons(a,b) else b)
  def foldRight[B](z: => B)(f: (A, => B) => B): B = uncons match {
    case Some((h,t)) => f(h, t.foldRight(z)(f))
    case None => z
  }
  def exists(p: A => Boolean): Boolean = foldRight(false)((a,b) => p(a) || b)
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a,b) => p(a) && b)
  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a,b) => Stream.cons(f(a), b))
  def map_2[B](f: A => B): Stream[B] = ???
  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((a,b) => if (p(a)) Stream.cons(a,b) else b)
  def append[AA >: A](as: Stream[AA]): Stream[AA] = foldRight(as)((a,b) => Stream.cons(a,b))
  def flatMap[AA >: A](f: A => Stream[AA]): Stream[AA] = foldRight(empty[AA])((a,b) => f(a).append(b))
}

object Stream {
  def empty[A]: Stream[A] = new Stream[A] {
    override def uncons: Option[(A, Stream[A])] = None
  }
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = new Stream[A] {
    lazy val uncons: Option[(A, Stream[A])] = Some(hd -> tl)
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

val s = Stream(1,2,3)
s.toList
s.take(2).toList
s.takeWhile(_ < 3).toList
s.takeWhile2(_ < 3).toList
s.exists(_ == 2)
s.forAll(_ > 0)
s.forAll(_ < 3)
s.map(_ * 2).toList
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

