import java.io.PrintWriter
import java.util.Date

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

var capital = Map("US" -> "Washington")
capital += ("Japan" -> "Tokyo")
capital

def fact(x:BigInt): BigInt = if (x==0) 1 else x*fact(x-1)
//fact(30)

(1 to 3).foreach(println)
for (i <- 1 to 3) println(i)

val gs = new Array[String](3)
gs(0) = "hello"
gs.update(1, "world")
gs

val nn2 = Array.apply("zero", "one")

val xs = List(3,2) ::: 1 :: Nil

xs.init
xs.last
xs.mkString(",")

(99, "T")._1

val jets = collection.mutable.Set("Boeing")
jets += "Airbus"
jets

//io.Source.fromFile("").getLines()

"x" * 10

class ChecksumAccumulator {
  private var sum = 0
}

object Summer {
  def main(args: Array[String]): Unit = {
    // ...
  }
}

Byte
Short
Int
Long
Char
//String
Float
Double
Boolean

0xcafebabe
//0777
3e3d
'A'
'\u0041'
"""W
  |T
  |""".stripMargin
Symbol("aSymbol")
(1).+(2L)
"o" indexOf 'o'
(2.0).unary_-
1 ^ 3
~1
-1 >> 31
-1 >>> 31
("he"+"llo") == "hello"
0 max 5
-2.abs

class Rational(n: Int, d: Int) {
  require(d != 0)
  def this(n: Int) = this(n, 1)
  def + (that: Rational) = ???
  def + (i: Int) = ???
}

var i = 0
do { i += 1 } while (i < 10)

for (i <- 1 until 4 if i < 3; s = i+"!") yield {
  val s2 = s
  s2
}

val num: Int = try { 0/0; 1 } catch { case ex => println(ex); 3 } finally { 2 }

val f = (x: Int) => x + 1
val g = (_: Int) + (_: Int)
def h(a:Int,b:Int = 0) = a+b

h(b = 3, a = 4)

val j = h _
val k = h(_: Int, 3)

k.apply(2)

def mkInc(m: Int) = (x:Int) => x+m
val inc9999 = mkInc(9999)
inc9999(1)

def echo(args:String*) = println(args.mkString)
echo(Array("foo","bar"): _*)

def curry(x:Int)(y:Int) = x + y
val curry1 = curry(1) _
curry1(2)

def withPrintWriter(f:java.io.File, op:PrintWriter => Unit) {}
withPrintWriter(
  new java.io.File("date.txt"),
  w => w.println(new Date)
)

def byNameAssert(p: => Boolean) = ???

abstract class Element {
  def contents: Array[String]
}

object Element {
  private final class ArrayElement(private val c: Array[String]) extends Element {
    override def contents: Array[String] = c
  }

  def elem(contents: Array[String]): Element =
    new ArrayElement(contents)
}

42.toString
42.hashCode
42 equals 42

def isEqual(x:Any,y:Any) = x == y
isEqual(421,421)

//val int: Int = null
def error(m:String): Nothing = throw new RuntimeException(m)

trait Philosophical {
  def philosophize = ???
}

class Animal
trait HasLegs
class Frog extends Animal with Philosophical with HasLegs
val ph: Philosophical = new Frog

abstract class IntQueue {
  def get(): Int
  def put(x:Int)
}

class BasicIntQueue extends IntQueue {
  private val buf = new ArrayBuffer[Int]()
  def get() = buf.remove(0)
  def put(x:Int) { buf += x }
}

trait Doubling extends IntQueue {
  abstract override def put(x: Int): Unit = {
    super.put(2*x)
  }
}

trait Incrementing extends IntQueue {
  abstract override def put(x: Int): Unit = {
    super.put(x+1)
  }
}

val queue = new BasicIntQueue with Incrementing with Doubling
queue.put(1)
queue.put(2)
queue.put(3)
queue.get()
queue.get()
queue.get()

assert(2 == 2)

sealed abstract class Expr
case class Number(num: Double) extends Expr

Number(1) match {
  case n @ Number(num) => num
  case _ => 0
}

val withDefault: Option[Int] => Int = {
  case Some(x) => x
  case None => 0
}

val pf = new PartialFunction[List[Int], Int] {
  override def apply(v1: List[Int]): Int = v1 match {
    case x :: y :: _ => y
  }

  override def isDefinedAt(x: List[Int]): Boolean = x match {
    case x :: y :: _ => true
    case _ => false
  }
}

Nil.isEmpty

val l = List(1) ::: List(2)
l.exists(_ == 1)
l zip l.indices
l.zipWithIndex.unzip
l.iterator.next
List.range(1,5).forall(_ > 0)
List.range(1,9,2)
List(1,2,3,4,5) partition (_ % 2 == 0)
List(1,2,3,-4,5) span (_ > 0)
List.fill(3,3)("hello")
List.tabulate(5)(n => n*n)
List.concat(l, List(3,4))
(l, List(3,4)).zipped.map(_*_)

val a = Array(5,4,3)
a(0)

val lb = new ListBuffer[Int]
lb += 1
3 +=: lb
lb.insert(1, 2)
lb.toList

val ab = new ArrayBuffer[Int]()
ab += 12
ab.trimStart(1)

Set.empty[String] ++ List(1)

val m = Map("i" -> 1)
m + ("ii" -> 2)
m ++ List("iii" -> 3, "v" -> 5)
m - "ii"
m -- List("i", "ii")
m.size
m.contains("i")
//m("ii")
m.keys
m.keySet
m.values
m.isEmpty

val mm = collection.mutable.Map("i" -> 1)
mm.getOrElseUpdate("ii", 2)

val ts = collection.immutable.TreeSet(9, 3, 1)
collection.immutable.TreeSet[String]() ++ List("q","x","a")

collection.mutable.Set.empty ++= ts
val (w,idx) = ("foo",1)

class Time {
  private[this] var h = 12
  def hour: Int = h
  def hour_=(x: Int) { h = x }
}

val time = new Time
time.hour = 10
time.hour

type Action = () => Unit

class SlowAppendQueue[T](elems: List[T]) {
  def head = elems.head
  def tail = new SlowAppendQueue(elems.tail)
  def enqueue(x:T) = new SlowAppendQueue(elems ::: List(x))
}

class Queue[+T] {
  def enqueue[U >: T](x:U) = ???
}

trait OutputChannel[-T] {
  def write(x:T)
}

trait Abstact {
  type T
}

class Concrete extends Abstact {
  type T = String
}

trait RationalTrait {
  val numerArg: Int
  val denomArg: Int
}

new {
  val numerArg = 1
  val denomArg = 2
} with RationalTrait

object twoThirds extends {
  val numerArg = 1
  val denomArg = 2
} with RationalTrait

lazy val x = { println("init"); "done" }

def using[T <: { def close(): Unit }, S](obj: T)(op: T => S) = {
  val result = op(obj)
  obj.close()
  result
}

object Color extends Enumeration {
  val Red = Value("Red")
}

object Direction extends Enumeration {
  val North, East = Value
}

Direction.values
Direction.East.id
Direction(1)

implicit def intToString(x: Int) = x.toString

class PreferredPrompt(val preference: String)
def greet(name: String)(implicit prompt: PreferredPrompt) = ???

def maxList[T <% Ordered[T]](elements: List[T]) = ???

for {
  x <- List(1,2)
  y <- List("one", "two")
} yield (x,y)

List(1,2).collect {
  case i: Int if i > 1 => i
}
Nil.nonEmpty
Nil.headOption

List(1,2,3) grouped 2
List(1,2,3) sliding 2

List(1,2,3) zipAll (List(1,2), 0, 0)
List(1,2,3) sameElements List(1,2,3)

1 +: List(2)
List(1) :+ 2
List(1) padTo (3, 0)
List(1,2,3) patch (1, List(10,11), 1)
List(1) updated (0, 0)
List(2,1).sorted
List(1,2,3) intersect List(2,3,4)
List(1,1,2).distinct

Set(1,2,3) & Set(2,3,4)
Set(1,2,3).empty
Set(1,2).subsetOf(Set(1,2,3))

1 #:: LazyList.empty

collection.immutable.Vector.empty :+ 1
IndexedSeq(1,2,3)

val s = mutable.Stack.empty[Int]
s.push(1)

var q = collection.immutable.Queue[Int]()
q.enqueue(1)

val redBlackTree = mutable.TreeMap.empty[Int,Int]
val bits = collection.immutable.BitSet.empty
bits + 2
bits(1)

val sb = new StringBuilder
s +=  'a'

val hm = mutable.HashMap.empty[Int,String]
hm += (1 -> "Foo")

object Email {
  def apply() = ???
  def unapply(s: String): Option[(String,String)] = None
}

"""(-)?""".r

//<a>This is some {"XML"}</a>