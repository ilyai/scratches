import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1
    case Cons(x,xs) => x * product(xs)
  }

  def tail[A](as: List[A]): List[A] = as match {
    case Nil => sys.error("Nil.tail")
    case Cons(_,t) => t
  }

  def drop[A](as: List[A], n: Int): List[A] =
    if (n <= 0) as
    else as match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n-1)
    }

  def dropWhile[A](as: List[A])(p: A => Boolean): List[A] =
    as match {
      case Nil => Nil
      case Cons(h, t) => if (p(h)) dropWhile(t)(p) else as
    }

  def setHead[A](as: List[A], h: A): List[A] = as match {
    case Nil => sys.error("setHead on empty list")
    case Cons(_, t) => Cons(h, t)
  }

  def append[A](l1: List[A], l2: List[A]): List[A] = l1 match {
    case Nil => l2
    case Cons(h,t) => Cons(h, append(t, l2))
  }

  def append2[A](l1: List[A], l2: List[A]): List[A] =
    foldRight(l1, l2)((a,b) => Cons(a,b))

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init of empty list")
    case Cons(_,Nil) => Nil
    case Cons(h,t) => Cons(h,init(t))
  }

  def init2[A](l: List[A]): List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    @tailrec
    def go(cur: List[A]): List[A] = cur match {
      case Nil => sys.error("init of empty list")
      case Cons(_,Nil) => List(buf.toList: _*)
      case Cons(h,t) => {
        buf += h
        go(t)
      }
    }
    go(l)
  }

//  @tailrec
  def foldRight[A,B](l: List[A], z: B)(f: (A,B) => B): B = l match {
    case Nil => z
    case Cons(h,t) => f(h, foldRight(t,z)(f)) // foldRight(t,f(h,z))(f)
  }

  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B,A) => B): B = l match {
    case Nil => z
    case Cons(h,t) => foldLeft(t,f(z,h))(f) // f(foldLeft(t,z)(f), h)
  }

  def sum2(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def product2(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

  def length[A](l: List[A]): Int = foldLeft(l, 0)((b,_) => b+1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil:List[A])((acc,h) => Cons(h,acc))

//  def foldLeft2[A,B](l: List[A], z: B)(f: (B,A) => B): B =
//    foldRight(reverse(l), z)((a,b) => f(b,a))

  def foldLeft2[A,B](l: List[A], z: B)(f: (B,A) => B): B =
    foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)

  def foldRight2[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(reverse(l), z)((b,a) => f(a,b))

  def flatten[A](ll: List[List[A]]): List[A] =
    foldRight(ll, Nil:List[A])(append)

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def add1(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x+1, add1(xs))
  }

  def doubleToString(xs: List[Double]): List[String] = xs match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x.toString, doubleToString(xs))
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), map(t)(f))
  }

  def filter[A](l: List[A])(p: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h,t) => if (p(h)) Cons(h, filter(t)(p)) else filter(t)(p)
  }

  def filter2[A](l: List[A])(p: A => Boolean): List[A] =
    flatMap(l)(a => if (p(a)) List(a) else Nil)

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = l match {
    case Nil => Nil
    case Cons(h,t) => append(f(h), flatMap(t)(f))
  }

  def addPairwise(l1: List[Int], l2: List[Int]): List[Int] = (l1,l2) match {
    case (Cons(h1,t1), Cons(h2, t2)) => Cons(h1+h2, addPairwise(t1,t2))
    case _ => Nil
  }

  def zipWith[A](l1: List[A], l2: List[A])(f: (A,A) => A): List[A] = (l1,l2) match {
    case (Cons(h1,t1), Cons(h2, t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
    case _ => Nil
  }

  @tailrec
  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {
    @tailrec
    def hasPrefix(l: List[A], pre: List[A]): Boolean = (l,pre) match {
      case (Cons(a,as),Cons(p,ps)) if a == p => hasPrefix(as,ps)
      case (_,Nil) => true
      case _ => false
    }
    (l,sub) match {
      case (Cons(a,as), Cons(s,ss)) =>
        if (a == s && hasPrefix(as,ss)) true else hasSubsequence(as,sub)
      case (_,Nil) => true
      case _ => false
    }
  }
}

val myIntList = Cons(1, Cons(3, Cons(5, Nil)))
val myDoubleList: List[Double] = Cons(1, Cons(3, Cons(5, Nil)))

List(1,2,3)
List.sum(myIntList)
List.sum2(myIntList)
List.product(myDoubleList)
List.product2(myDoubleList)
List.tail(myIntList)
List.drop(myIntList, 1)
List.dropWhile(myIntList)(_ < 4)
List.setHead(myIntList, 10)
List.append(myIntList, List(7,11))
List.append2(myIntList, List(7,11))
List.init(myIntList)
List.init2(myIntList)
List.foldRight(myIntList, Nil:List[Int])(Cons(_,_))
List.foldRight2(myIntList, Nil:List[Int])(Cons(_,_))
List.foldLeft(myIntList, Nil:List[Int])((a,b) => Cons(b,a))
List.foldLeft2(myIntList, Nil:List[Int])((a,b) => Cons(b,a))
List.length(myIntList)
List.reverse(myIntList)
List.flatten(List(myIntList,List(7,11)))
List.add1(myIntList)
List.doubleToString(myDoubleList)
List.map(myIntList)(_ + 1)
List.filter(myIntList)(_ % 2 == 0)
List.filter2(myIntList)(_ % 2 == 0)
List.flatMap(myIntList)(i => List(i,i))
List.addPairwise(List(1,2,3), List(4,5,6))
List.zipWith(List(1,2,3), List(4,5,6))(_ + _)
List.hasSubsequence(List(1,2,3,4), List(1,2))
List.hasSubsequence(List(1,2,3,4), List(2,3))
List.hasSubsequence(List(1,2,3,4), List(4))
List.hasSubsequence(List(1,2,3,4), List(1,2,3,4))
List.hasSubsequence(List(1,1,2,3,4), List(1,2,3,4))
List.hasSubsequence(List(1,2,3,4), Nil)
List.hasSubsequence(Nil, List(1,2,3,4))
List.hasSubsequence(List(1,2,3,4), List(2,4))
List.hasSubsequence(List(1,2,3,4), List(2,3,5))
List.hasSubsequence(List(1,2,3,4), List(5))

val x = List(1,2,3,4,5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h,t) => h + List.sum(t)
  case _ => 101
}

val myScalaList: scala.List[Int] = scala.List(1,2,3)
myScalaList.take(1)
myScalaList.takeWhile(_ < 4)
myScalaList.forall(_ > 0)
myScalaList.exists(_ > 3)
myScalaList.scanLeft(0)(_ + _)
myScalaList.scanRight(0)(_ + _)

val p = ("Bob", 42)
p._1
p._2
p match { case (_,b) => b }

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)
  }

  def size2[A](t: Tree[A]): Int = fold(t)(_ => 1)(_ + _ + 1)

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(a) => a
    case Branch(l,r) => maximum(l) max maximum(r)
  }

  def maximum2(t: Tree[Int]): Int = fold(t)(a => a)(_ max _)

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l,r) => 1 + (depth(l) max depth(r))
  }

  def depth2[A](t: Tree[A]): Int = fold(t)(_ => 0)((d1,d2) => (d1 max d2) + 1)

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }

  def map2[A,B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)):Tree[B])(Branch(_,_))

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }
}

val myIntTree = Branch(Leaf(1), Branch(Leaf(3), Leaf(5)))
Tree.size(myIntTree)
Tree.size2(myIntTree)
Tree.maximum(myIntTree)
Tree.maximum2(myIntTree)
Tree.depth(myIntTree)
Tree.depth2(myIntTree)
Tree.map(myIntTree)(_ + 1)
Tree.map2(myIntTree)(_ + 1)
