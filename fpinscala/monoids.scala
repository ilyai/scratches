trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

val stringMonoid = new Monoid[String] {
  override def op(a1: String, a2: String): String = a1 + a2
  override def zero: String = ""
}

def listMonoid[A] = new Monoid[List[A]] {
  override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
  override def zero: List[A] = Nil
}

val intAddition = new Monoid[Int] {
  override def op(a1: Int, a2: Int): Int = a1 + a2
  override def zero: Int = 0
}

val booleanOr = new Monoid[Boolean] {
  override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
  override def zero: Boolean = false
}

def optionMonoid[A] = new Monoid[Option[A]] {
  override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
  override def zero: Option[A] = None
}

def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
  override def op(f: A => A, g: A => A): A => A = f compose g
  override def zero: A => A = a => a
}

def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A,B)] =
  new Monoid[(A, B)] {
    override def op(a1: (A, B), a2: (A, B)): (A, B) = (A.op(a1._1, a2._1), B.op(a1._2,a2._2))
    override def zero: (A, B) = (A.zero, B.zero)
  }

def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] =
  new Monoid[A => B] {
    override def op(f: A => B, g: A => B): A => B = a => B.op(f(a), g(a))
    override val zero: A => B = _ => B.zero
  }

def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K,V]] =
  new Monoid[Map[K, V]] {
    override def op(a1: Map[K, V], a2: Map[K, V]): Map[K, V] =
      (a1.keySet ++ a2.keySet).foldLeft(zero) { (acc,k) =>
        acc.updated(k, V.op(a1.getOrElse(k, V.zero),
                            a2.getOrElse(k, V.zero)))
      }
    override def zero: Map[K, V] = Map[K,V]()
  }

def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
  def op(x: A, y: A): A = m.op(y, x)
  val zero = m.zero
}

val words = List("Hic", "Est", "Index")

val s = words.foldRight(stringMonoid.zero)(stringMonoid.op)
val t = words.foldLeft(stringMonoid.zero)(stringMonoid.op)

val wordsMonoid: Monoid[String] = new Monoid[String] {
  override def op(a1: String, a2: String): String = s"${a1} ${a2}"
  override def zero: String = ""
}

words.foldRight(wordsMonoid.zero)(wordsMonoid.op)

def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B =
  as.foldLeft(m.zero)((b,a) => m.op(b, f(a)))

def foldRight[A,B](as: List[A])(z: B)(f: (A,B) => B): B =
  foldMap(as, endoMonoid[B])(f.curried)(z)

def foldLeft[A,B](as: List[A])(z: B)(f: (B,A) => B): B =
  foldMap(as, endoMonoid[B])(a => b => f(b,a))(z)

sealed trait WC
case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC

val wcMonoid: Monoid[WC] = new Monoid[WC] {
  override def op(a1: WC, a2: WC): WC = (a1,a2) match {
    case (Stub(c), Stub(d)) => Stub(c + d)
    case (Stub(c), Part(l,w,r)) => Part(c+l, w, r)
    case (Part(l,w,r), Stub(c)) => Part(l, w, r+c)
    case (Part(l1,w1,r1), Part(l2,w2,r2)) =>
      Part(l1, w1 + (if ((r1 + l2).isEmpty) 0 else 1) + w2, r2)
  }

  override def zero: WC = Stub("")
}

List("lorem", "ipsum", "dolor", "sit").foldLeft("")(_ + _)

def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
  if (v.isEmpty)
    m.zero
  else if (v.length == 1)
    f(v(0))
  else {
    val (l,r) = v.splitAt(v.length/2)
    m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
  }

def bag[A](as: IndexedSeq[A]): Map[A,Int] =
  foldMapV(as, mapMergeMonoid[A,Int](intAddition))((a: A) => Map(a -> 1))

foldMapV(IndexedSeq("lorem", "ipsum", "dolor", "sit"), stringMonoid)(endoMonoid.zero)

List(1,2,3).foldRight(0)(_ + _)

trait Foldable[F[_]] {
  def foldRight[A,B](as: F[A])(z: B)(f: (A,B) => B): B =
    foldMap(as)(f.curried)(endoMonoid[B])(z)

  def foldLeft[A,B](as: F[A])(z: B)(f: (B,A) => B): B =
    foldMap(as)(a => (b: B) => f(b,a))(dual(endoMonoid[B]))(z)

  def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a,b) => mb.op(f(a),b))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List[A]())(_ :: _)
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    foldMapV(as, mb)(f)
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
}

object OptionFoldable extends Foldable[Option] {
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) = as match {
    case None => z
    case Some(a) => f(a,z)
  }
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) = as match {
    case None => z
    case Some(a) => f(z,a)
  }
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case None => mb.zero
    case Some(a) => f(a)
  }
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =  as match {
    case Leaf(a) => f(a)
    case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
  }
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) = as match {
    case Leaf(a) => f(z, a)
    case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
  }
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) = as match {
    case Leaf(a) => f(a, z)
    case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
  }
}

val M: Monoid[Map[String, Map[String,Int]]] = mapMergeMonoid(mapMergeMonoid(intAddition))
val m1 = Map("o1" -> Map("i1" -> 1, "i2" -> 2))
val m2 = Map("o1" -> Map("i2" -> 3))
val m3 = M.op(m1,m2)

val m = productMonoid(intAddition, intAddition)
val p = ListFoldable.foldMap(List(1,2,3,4))(a => (1,a))(m)
val mean = p._1 / p._2.toDouble

//def monoid[A](implicit A: Monoid[A]): Monoid[A] = A

case class Sum(value: Int)
implicit val sumMonoid: Monoid[Sum] = new Monoid[Sum] {
  override def op(a1: Sum, a2: Sum): Sum = Sum(a1.value + a2.value)
  override def zero: Sum = Sum(0)
}

ListFoldable.foldMap(List(1,2,3,4))(Sum)(sumMonoid)