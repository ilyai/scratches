sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](as: List[A]): List[A] = as match {
    case Nil => sys.error("tail of Nil")
    case Cons(_, t) => t
  }

  def setHead[A](h: A, as: List[A]): List[A] = as match {
    case Nil => sys.error("setHead of empty list")
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case _ if n == 0 => l
    case Cons(_, t) => drop(t, n-1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
  }

  def dropWhile2[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Cons(h,t) if f(h) => dropWhile2(t)(f)
    case _ => as
  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(h,Cons(_,Nil)) => Cons(h, Nil)
    case Cons(h, t) => Cons(h, init(t))
    case _ => l
  }

//  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B = as match {
//    case Cons(h,t) => foldRight(t, f(h,z))(f)
//    case _ => z
//  }

  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B = as match {
    case Cons(h,t) => f(h, foldRight(t, z)(f))
    case _ => z
  }

  def sum2(ns: List[Int]) = foldRight(ns, 0)(_ + _)
  def product2(ns: List[Int]) = foldRight(ns, 1)(_ * _)
  def length[A](as: List[A]): Int = foldRight(as, 0)((_,b) => b+1)

  def foldLeft[A,B](as: List[A], z: B)(f: (B,A) => B): B = as match {
    case Nil => z
    case Cons(h,t) => foldLeft(t, f(z,h))(f)
  }

  def sum3(ns: List[Int]) = foldLeft(ns, 0)(_ + _)
  def product3(ns: List[Int]) = foldLeft(ns, 1)(_ * _)

  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, Nil: List[A])((a,b) => Cons(b,a))
  }

  def append[A](l: List[A], r: List[A]) = {
    foldRight(l, r)(Cons(_,_))
  }

  def concat[A](ll: List[List[A]]): List[A] = {
    foldRight(ll, Nil: List[A])(append)
  }

  def addOne(xs: List[Int]): List[Int] = {
    foldRight(xs, Nil: List[Int])((a,b) => Cons(a+1, b))
  }

  def doubleToString(xs: List[Double]): List[String] = {
    foldRight(xs, Nil: List[String])((a,b) => Cons(a.toString, b))
  }

  def map[A,B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, Nil:List[B])((a,b) => Cons(f(a), b))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, Nil:List[A])((a,b) => if (f(a)) Cons(a,b) else b)
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    foldRight(as, Nil:List[B])((a,b) => append(f(a),b))
  }

  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(a => if (f(a)) Cons(a, Nil) else Nil)
  }

  def addPairwise(l: List[Int], r: List[Int]): List[Int] = (l,r) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(a, t1), Cons(b, t2)) => Cons(a+b, addPairwise(t1, t2))
  }

  def zipWith[A,B,C](l: List[A], r: List[B])(f: (A,B) => C): List[C] = (l,r) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(a, t1), Cons(b, t2)) => Cons(f(a,b), zipWith(t1, t2)(f))
  }

  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    case (_, Nil) => true
    case (Cons(h1,t1), Cons(h2,t2)) => if (h1 == h2) startsWith(t1,t2) else false
    case _ => false
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(h,t) => hasSubsequence(t,sub)
  }

}

List.sum(Cons(1, Cons(2, Cons(3, Nil))))
List.product(Cons(4.0, Cons(2.0, Cons(3.0, Nil))))

val ex3: List[String] = Cons("a", Cons("b", Nil))

List(1,2,3) match { case _ => 42 }
//List(1,2,3) match { case Cons(h,_) => h }
//List(1,2,3) match { case Cons(_,t) => t }
//List(1,2,3) match { case Nil => 42 }

val x = List(1,2,3,4,5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t) => h + List.sum(t)
  case _ => 101
}

List.dropWhile(List(1,2,3), (x:Int) => x > 2)
List.dropWhile2(List(1,2,3))(_ > 2)
List.foldRight(List(8,9,10), Nil:List[Int])(Cons(_,_))

List.sum3(List(4,5,6))
List.product3(List(4,5,6))

List.reverse(List(1,2,3))
List.concat(List(List(1,2,3), List(4,5,6)))
List.addOne(List(1,2,3))
List.doubleToString(List(1.3, 6.3, 3.1))
List.map(List(1,2,3))(_ * 3)
List.filter(List(1,2,3))(_ > 2)
List.filterViaFlatMap(List(1,2,3))(_ > 2)
List.flatMap(List(1,2,3))(i => List(i,i))
List.addPairwise(List(1,2,3), List(4,5,6))
List.zipWith(List(1,2,3), List(4,5,6))(_ + _)
List.hasSubsequence(List(1,2,3,4,5), List(2,3,4))

val p = ("Bob", 42)
p match { case (a,b) => b }

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => size(l) + size(r) + 1
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l,r) => maximum(l).max(maximum(r))
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l,r) => depth(l).max(depth(r)) + 1
  }

  def map[A,B](t: Tree[A], f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l,r) => Branch(map(l,f), map(r,f))
  }

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def size2[A](t: Tree[A]): Int = fold(t)(_ => 1)(_ + _ + 1)
  def maximum2(t: Tree[Int]): Int = fold(t)(a => a)(_ max _)
  def depth2[A](t: Tree[A]): Int = fold(t)(_ => 0)((x,y) => 1 + (x max y))
  def map2[A,B](t: Tree[A], f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_,_))
}

Tree.size(Branch(Leaf(1), Branch(Branch(Leaf(2), Leaf(3)), Leaf(4))))
Tree.size2(Branch(Leaf(1), Branch(Branch(Leaf(2), Leaf(3)), Leaf(4))))
Tree.maximum(Branch(Leaf(1), Branch(Branch(Leaf(2), Leaf(3)), Leaf(4))))
Tree.maximum2(Branch(Leaf(1), Branch(Branch(Leaf(2), Leaf(3)), Leaf(4))))
Tree.depth(Branch(Leaf(1), Branch(Branch(Leaf(2), Leaf(3)), Leaf(4))))
Tree.depth2(Branch(Leaf(1), Branch(Branch(Leaf(2), Leaf(3)), Leaf(4))))
Tree.map(Branch(Leaf(1), Branch(Branch(Leaf(2), Leaf(3)), Leaf(4))), (x:Int) => x * 2)
Tree.map2(Branch(Leaf(1), Branch(Branch(Leaf(2), Leaf(3)), Leaf(4))), (x:Int) => x * 2)
Tree.fold(Branch(Leaf(1), Branch(Branch(Leaf(2), Leaf(3)), Leaf(4))))(_ * 2)(_ + _)
