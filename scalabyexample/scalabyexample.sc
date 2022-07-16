import scala.sys.error

def sort(xs: Array[Int]): Unit = {
  def swap(i: Int, j: Int): Unit = {
    val t = xs(i)
    xs(i) = xs(j)
    xs(j) = t
  }
  def sort1(l: Int, r: Int): Unit = {
    val pivot = xs((l+r) / 2)
    var i = l; var j = r
    while (i <= j) {
      while (xs(i) < pivot) i += 1
      while (xs(j) > pivot) j -= 1
      if (i <= j) {
        swap(i, j)
        i += 1
        j -= 1
      }
    }
    if (l < j) sort1(l, j)
    if (j < r) sort1(i, r)
  }
  sort1(0, xs.length-1)
}

def sort2(xs: Array[Int]): Array[Int ] = {
  if (xs.length <= 1) xs
  else {
    val pivot = xs(xs.length / 2)
    Array.concat(
      sort2(xs filter (pivot >)),
      xs filter (pivot ==),
      sort2(xs filter (pivot <))
    )
  }
}

val xs = Array(1,3,9,0,5)
sort2(xs)
xs

def loop: Int = loop
def constOne(x: Int, y: => Int) = 1
constOne(1, loop)

def sqrt(x: Double): Double = {
  def improve(guess: Double, x: Double) =
    (guess + x / guess) / 2
  def isGoodEnough(guess: Double, x: Double) =
    math.abs((guess * guess) - x) < .001
  def sqrtIter(guess: Double, x: Double): Double =
    if (isGoodEnough(guess, x)) guess
    else sqrtIter(improve(guess, x), x)
  sqrtIter(1.0, x)
}
sqrt(2)

def sum(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 0 else f(a) + sum(f)(a+1, b)

def sum2(f: Int => Int)(a: Int, b: Int): Int = {
  def iter(a: Int, result: Int): Int = {
    if (a > b) result
    else iter(a+1, f(a)+result)
  }
  iter(a, 0)
}

def id(x: Int) = x
sum(id _)(1, 100)
sum2(id)(1, 100)

class Rational(n: Int, d: Int) {
  private def gcd(x: Int, y: Int): Int = {
    if (x == 0) y
    else if (x < 0) gcd(-x, y)
    else if (y < 0) -gcd(x, -y)
    else gcd(y % x, x)
  }
  private val g = gcd(n, d)
  val numer: Int = n/g
  val denom: Int = d/g
  def +(that: Rational) =
    new Rational(numer * that.denom + that.numer * denom,
      denom * that.denom)
  def -(that: Rational) =
    new Rational(numer * that.denom - that.numer * denom,
      denom * that.denom)
  def *(that: Rational) =
    new Rational(numer * that.numer, denom * that.denom)
  def /(that: Rational) =
    new Rational(numer * that.denom, denom * that.numer)
  override def toString: String = numer + "/" + denom
}

var x = new Rational(0,1)
for (i <- 1 to 10) {
  x += new Rational(1,i)
}
x

abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
}

//trait IntSet {
//  def incl(x: Int): IntSet
//  def contains(x: Int): Boolean
//}

abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  override def isZero: Boolean = true
  override def predecessor: Nat = error("negative number")
  override def successor: Nat = new Succ(Zero)
  def + (that: Nat): Nat = that
  def - (that: Nat): Nat =
    if (that.isZero) Zero
    else error("negative number")
}

case class Succ(x: Nat) extends Nat {
  override def isZero: Boolean = false
  override def predecessor: Nat = x
  override def successor: Nat = new Succ(this)
  def + (that: Nat): Nat = x + that.successor
  def - (that: Nat): Nat =
    if (that.isZero) this
    else x - that.predecessor
}

Succ(Zero) + Succ(Succ(Zero))

//abstract class Expr {
//  def eval: Int
//}
//
//class Number(n: Int) extends Expr {
//  override def eval: Int = n
//}
//
//class Sum(e1: Expr, e2: Expr) extends Expr {
//  override def eval: Int = e1.eval + e2.eval
//}
//
//class Prod(e1: Expr, e2: Expr) extends Expr {
//  override def eval: Int = e1.eval + e2.eval
//}

abstract class Expr {
  def eval: Int = this match {
    case Number(n) => n
    case Sum(l,r) => l.eval + r.eval
  }
}
case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr

Sum(Sum(Number(1), Number(2)), Number(3)).eval

abstract class IntTree
case object EmptyTree extends IntTree
case class Node(elem: Int, left: IntTree, right: IntTree) extends IntTree

def contains(t: IntTree, v: Int): Boolean = t match {
  case EmptyTree => false
  case Node(e,l,r) => if (e == v) true else contains(l,v) || contains(r,v)
}

abstract class Stack[A] {
  def push(x: A): Stack[A] = new NonEmptyStack(x, this)
  def isEmpty: Boolean
  def top: A
  def pop: Stack[A]
}

class EmptyStack[A] extends Stack[A] {
  override def isEmpty: Boolean = true
  override def top = error("EmptyStack.top")
  override def pop = error("EmptyStack.pop")
}
class NonEmptyStack[A](elem: A, rest: Stack[A]) extends Stack[A] {
  override def isEmpty: Boolean = false
  override def top: A = elem
  override def pop: Stack[A] = rest
}

val x1 = new EmptyStack[Int]
val y1 = x1.push(1).push(2)
y1.pop.top

def isPrefix[A](p: Stack[A], s: Stack[A]): Boolean = {
  p.isEmpty ||
    p.top == s.top && isPrefix(p.pop, s.pop)
}

val s1 = new EmptyStack[String].push("abc")
val s2 = new EmptyStack[String].push("abx").push("abc")
isPrefix(s1,s2)

//abstract class Set[A] {
//  def incl(x: A): Set[A]
//  def contains(x: A): Boolean
//}

trait Ordered[A] {
  def compare(that: A): Int
  def < (that: A): Boolean = (this compare that) < 0
  def > (that: A): Boolean = (this compare that) > 0
  def <= (that: A): Boolean = (this compare that) <= 0
  def >= (that: A): Boolean = (this compare that) >= 0
  def compareTo(that: A): Int = compare(that)
}

trait Set[A <: Ordered[A]] {
  def incl(x: A): Set[A]
  def contains(x: A): Boolean
}

class EmptySet[A <: Ordered[A]] extends Set[A] {
  override def contains(x: A): Boolean = false
  override def incl(x: A): Set[A] =
    new NonEmptySet[A](x, new EmptySet[A], new EmptySet[A])
}

class NonEmptySet[A <: Ordered[A]](elem: A, left: Set[A], right: Set[A]) extends Set[A] {
  override def contains(x: A): Boolean = {
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  }
  override def incl(x: A): Set[A] = {
    if (x < elem) new NonEmptySet(elem, left incl x, right)
    else if (x > elem) new NonEmptySet(x, new EmptySet[A], new EmptySet[A])
    else this
  }
}

case class Num(value: Double) extends Ordered[Num] {
  def compare(that: Num): Int = {
    if (this.value < that.value) -1
    else if (this.value > that.value) 1
    else 0
  }
}

val s = new EmptySet[Num].incl(Num(1.0)).incl(Num(2.0))
s.contains(Num(1.5))
s.contains(Num(2.0))

abstract class MyStack[+A] {
  def push[B >: A](x: B): MyStack[B]
}

object MyEmptyStack extends MyStack[Nothing] {
  override def push[B >: Nothing](x: B): MyStack[B] = ???
}

def divmod(x: Int, y: Int) = new Tuple2[Int,Int](x / y, x % y)
divmod(1,2) match {
  case (n,d) => s"quotient:${n}, rest:${d}"
}

val f: (AnyRef => Int) = x => x.hashCode()
val g: (String => Int) = f
g("abc")

val fruit: List[String] = List("apples", "oranges", "pears")
val nums: List[Int] = 1 :: 2 :: 3 :: Nil
val empty = Nil

empty.isEmpty
fruit.head
fruit.tail.head
nums ::: nums

def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  case List() => List(x)
  case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
}

def msort[A](less: (A,A) => Boolean)(xs: List[A]): List[A] = {
  def merge(xs1: List[A], xs2: List[A]): List[A] = {
    if (xs1.isEmpty) xs2
    else if (xs2.isEmpty) xs1
    else if (less(xs1.head, xs2.head)) xs1.head :: merge(xs1.tail, xs2)
    else xs2.head :: merge(xs1, xs2.tail)
  }
  val n = xs.length/2
  if (n == 0) xs
  else merge(msort(less)(xs take n), msort(less)(xs drop n))
}

val intSort = msort((x:Int, y:Int) => x < y) _
intSort(List(5,7,1,3))

def column[A](xs: List[List[A]], index: Int): List[A] =
  xs map (row => row(index))

def range(from: Int, end: Int): List[Int] =
  if (from >= end) Nil else from :: range(from+1, end)

def isPrime(n:Int) =
  List.range(2,n) forall (n % _ != 0)

isPrime(100)
isPrime(11)

def foldLeft[A,B](z:B)(op: (B,A) => B)(l: List[A]): B = l match {
  case Nil => z
  case x :: xs => foldLeft(op(z, x))(op)(xs)
}

def reduceLeft[A](op: (A,A) => A)(l: List[A]): A = l match {
  case Nil => error("Nil.reduceLeft")
  case h :: t => op(h, reduceLeft(op)(t))
}

def reduceRight[A](op: (A,A) => A)(l: List[A]): A = l match {
  case Nil => error("Nil.reduceRight")
  case x :: Nil => x
  case x :: xs => op(x, reduceRight(op)(xs))
}

def flatten[A](xs: List[List[A]]): List[A] =
  xs.foldRight(Nil:List[A])(_ ::: _)

def reverse[A](xs: List[A]) =
  foldLeft(Nil:List[A])((acc,x:A) => x :: acc)(xs)

reverse(List(1,2,3))

List.range(1,10)
  .flatMap(i => List.range(1,i).map(x => (i,x)))
  .filter(p => isPrime(p._1+p._2))

for {
  i <- List.range(1,10)
  j <- List.range(1,i)
  if isPrime(i+j)
} yield (i,j)

case class Book(title: String, authors: List[String])
val books = List(
  Book("Principles of Compiler Design",
    List("Aho, Alfred", "Ullman, Jeffrey"))
)

for (b <- books; a <- b.authors if a startsWith "Ullman")
  yield b.title

books
  .flatMap(b => b.authors.withFilter(a => a.startsWith("Ullman"))
    .map(_ => b.title))

def removeDuplicates[A](xs: List[A]): List[A] =
  if (xs.isEmpty) xs
  else xs.head :: removeDuplicates(xs.tail filter (x => x != xs.head))

removeDuplicates(List(1,2,3,2))

object Demo {
  def map[A,B](xs: List[A], f: A => B): List[B] =
    for (x <- xs) yield f(x)
  def flatMap[A,B](xs: List[A], f: A => List[B]): List[B] =
    for (x <- xs; y <- f(x)) yield y
  def filter[A](xs: List[A], p: A => Boolean): List[A] =
    for (x <- xs if p(x)) yield x
}

class BankAccount {
  private var balance = 0
  def deposit(amount: Int): Unit = {
    if (amount > 0) balance += amount
  }
  def withdraw(amount: Int): Int =
    if (0 < amount && amount <= balance) {
      balance -= amount
      balance
    } else error("insufficient funds")
}

val account = new BankAccount
account deposit 50
account withdraw 20
account withdraw 20
//account withdraw 15

def whileLoop(condition: => Boolean)(command: => Unit): Unit = {
  if (condition) {
    command
    whileLoop(condition)(command)
  } else ()
}

type Action = () => Unit
class Wire {
  private var sigVal = false
  private var actions: List[Action] = List()
  def getSignal = sigVal
  def setSignal(s: Boolean) =
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  def addAction(a: Action): Unit = {
    actions = a :: actions
    a()
  }
}

Stream.cons(1, Stream.cons(2, Stream.empty))
LazyList.cons(1, LazyList.cons(2, LazyList.empty))

def rangeST(start: Int, end: Int): Stream[Int] =
  if (start >= end) Stream.empty
  else Stream.cons(start, rangeST(start+1, end))

rangeST(1000, 10000).filter(isPrime).head

val it: Iterator[Int] = Iterator.range(1,10)
while (it.hasNext) {
  val x = it.next()
  println(x*x)
}

Iterator.empty
Iterator.from(List(1,2,3)).foreach(println)

Iterator.from(List(1,2,3))
  .zip(Iterator.from(0))
  .filter { case (x,i) => x < 2 }
  .map { case (x,i) => i }
  .foreach(println)


object Db {
  val table = Map(
    1 -> (1, "Haruki Murakami", -1),
    2 -> (2, "Milan Kundera", 1)
  )
  def team(id: Int) = {
    for (rec <- table.values.toList if rec._3 == id)
      yield recToEmployee(rec)
  }
  def get(id: Int) = recToEmployee(table(id))
  private def recToEmployee(rec: (Int,String,Int)) = {
    println(s"[db] fetching ${rec._1}")
    Employee(rec._1,rec._2,rec._3)
  }
}

case class Employee(id: Int,
                    name: String,
                    managerId: Int) {
  lazy val manager: Employee = Db.get(managerId)
  lazy val team: List[Employee] = Db.team(id)
}

val emp = Employee(1, "John Doe", 1)
emp.manager
emp.team

emp.manager
emp.team

abstract class SemiGroup[A] {
  def add(x:A, y: A): A
}

abstract class Monoid[A] extends SemiGroup[A] {
  def unit: A
}

implicit object stringMonoid extends Monoid[String] {
  def add(x: String, y: String): String = x.concat(y)
  def unit: String = ""
}

implicit object intMonoid extends Monoid[Int] {
  def add(x: Int, y: Int): Int = x + y
  def unit: Int = 0
}

def sum[A](xs: List[A])(implicit m: Monoid[A]): A =
  if (xs.isEmpty) m.unit
  else m.add(xs.head, sum(xs.tail)(m))

sum[Int](List(1,2,3))
sum(List("a","b","c"))

implicit def int2ordered(x: Int): Ordered[Int] = new Ordered[Int] {
  override def compare(y: Int): Int = {
    if (x < y) -1
    else if (x > y) 1
    else 0
  }
}

def mergeWithViewBounds[A <% Ordered[A]](xs: List[A], ys: List[A]): List[A] =
  if (xs.isEmpty) ys
  else if (ys.isEmpty) xs
  else if (xs.head < ys.head) xs.head :: mergeWithViewBounds(xs.tail, ys)
  else ys.head :: mergeWithViewBounds(xs, ys.tail)

def merge[A](xs: List[A], ys: List[A])(implicit c: A => Ordered[A]): List[A] =
  if (xs.isEmpty) ys
  else if (ys.isEmpty) xs
  else if (c(xs.head) < ys.head) xs.head :: merge(xs.tail, ys)
  else ys.head :: merge(xs, ys.tail)(c)

abstract class Term
case class Var(x: String) extends Term {
  override def toString: String = x
}

sealed abstract class Type
case class Tyvar(a: String) extends Type {
  override def toString: String = a
}

object typeInfer {
  private var n: Int = 0
  def newTyvar(): Type = {
    n += 1
    Tyvar("a" + n)
  }
}

abstract class Subst extends (Type => Type) {
  def lookup(x: Tyvar): Type
  def apply(t: Type): Type = t match {
    case tv @ Tyvar(a) =>
      val u = lookup(tv)
      if (t == u) t
      else apply(u)
  }
}

class Lock {
  var available = true
  def acquire = synchronized {
    while (!available) wait()
    available = false
  }
  def release = synchronized {
    available = true
    notify()
  }
}

class LinkedList[A] {
  var elem: A = _
  var next: LinkedList[A] = null
}

abstract class Job {
  type T
  def task: T
  def ret(x: T)
}

def future[A](p: => A): Job = {
  new Job {
    type T = A
    def task: A = p
    def ret(x:A) = ???
  }
}

abstract class Mailbox {
  def send(msg: Any)
  def receive[A](f: PartialFunction[Any,A]): A
  def receiveWithin[A](msec: Long)(f: PartialFunction[Any,A]): A
}

abstract class Actor {
  def act(): Unit
  def run(): Unit = act()
  def send(msg: Any) = ???
  def !(msg: Any) = send(msg)
}