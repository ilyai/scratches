import scala.collection.{GenTraversable, LinearSeq, SortedSet, mutable}
import scala.collection.immutable.HashMap
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

Traversable(1,2,3)
/**/ Iterable("x", "y", "z")
/***/ Seq(1,2,3)
/****/ IndexedSeq(1.0, 2.0)
/*****/ Vector(1,2,3)
/****/ LinearSeq(1,2,3)
/*****/ List(1,2,3)
/****/ mutable.Buffer(1,2,3)
/*****/ ListBuffer(1,2,3)
/*****/ ArrayBuffer(1,2,3)
/***/ Set("one", "two", "three")
/****/ SortedSet("one", "two", "three")
/***/ Map("x" -> 24, "y" -> 25)
/****/ HashMap("x" -> 24, "y" -> 25, "z" -> 26)

val tr = Traversable(1,2,3)
tr foreach(println(_))

tr ++ Traversable(4,5)
tr map(_ * 2)
tr flatMap(x => Traversable(0 to x))
tr collect {
  case x if x < 2 => x
}
tr.toArray
tr.toList
//tr.toInterable
tr.toSeq
tr.toIndexedSeq
tr.toStream
tr.toSet
//tr.toMap
val buf = collection.mutable.ArrayBuffer[Int]()
tr copyToBuffer buf
buf
val arr = Array(100)
tr copyToArray(arr, 0, 3)
tr.isEmpty
tr.nonEmpty
tr.size
tr.hasDefiniteSize
tr.head
tr.headOption
tr.last
tr.lastOption
tr find (_ == 1)
tr.tail
tr.init
tr slice (0,1)
tr take 2
tr drop 2
tr takeWhile (_ < 2)
tr dropWhile (_ > 2)
tr filter (_ % 2 == 0)
tr withFilter(_ % 2 == 0)
tr.filterNot(_ % 2 == 0)
tr splitAt 2
tr span (_ < 2)
tr partition (_ < 2)
tr groupBy (_ < 2)
tr forall (_ < 10)
tr exists (_ > 2)
tr count (_ > 2)
(0 /: tr)(_ + _)
(tr :\ 0)(_ + _)
tr.foldLeft(0)(_ + _)
tr.foldRight(0)(_ + _)
tr reduceLeft (_ + _)
tr reduceRight (_ + _)
tr.sum
tr.product
tr.min
tr.max
//tr.addString(new StringBuilder(), 0, ",", 3)
tr.mkString
tr.stringPrefix
tr.view
tr.view(0,1)

val it = Iterable(1,2,3)
it.iterator
it grouped 2
it sliding 2
it.takeRight(2)
it.dropRight(2)
it zip Iterable(4,5,6)
it zipAll (Iterable(4,5), 9, 10)
it.zipWithIndex
it.sameElements(Iterable(1,2,3))

case class Branch(left: Tree, right: Tree) extends Tree
case class Node(elem: Int) extends Tree

sealed abstract class Tree extends Traversable[Int] {
  override def foreach[U](f: (Int) => U) = this match {
    case Node(elem) => f(elem)
    case Branch(l,r) =>
      l foreach f
      r foreach f
  }
}

//sealed abstract class Tree extends Iterable[Int] {
//  override def iterator = this match {
//    case Node(elem) => Iterator.single(elem)
//    case Branch(l, r) => l.iterator ++ r.iterator
//  }
//}

Seq(1,2,3)(1) == 2
Set('a', 'b', 'c')('b')
Map('a' -> 1)('a')

val sq = Seq(1,2,3)
sq(1)
sq.isDefinedAt(1)
sq.length
sq.lengthCompare(2)
sq.indices
sq.indexOf(2)
sq.lastIndexOf(2)
sq.indexOfSlice(Seq(1,2))
sq.lastIndexOfSlice(Seq(2,3))
sq.indexWhere(_ > 2)
sq.segmentLength(_ > 1, 0)
sq.prefixLength(_ > 2)
0 +: sq
sq :+ 4
sq padTo (5, 0)
sq patch (0, Seq(6,7), 2)
sq updated (1, 0)
//sq(0) = 1
sq.sorted
//sq.sortWith(lessThan)
//sq.sortBy(f)
sq.reverse
sq.reverseIterator
sq.reverseMap(_ * 2)
sq.startsWith(Seq(1,2))
sq.endsWith(Seq(2,3))
sq.contains(2)
sq.containsSlice(Seq(2,3))
sq.corresponds(Seq(1,2,3))((x,y) => x == y)
sq.intersect(Seq(2,3,4))
sq.diff(Seq(1,2))
sq.union(Seq(1,2))
sq.distinct

val abuf = ArrayBuffer(1,2,3,4,5)
val lbuf = ListBuffer(1,2,3)
abuf += 4
abuf += (4,5,6,7,8)
abuf ++= sq
0 +=: abuf
sq ++=: abuf
abuf insert (3, 3)
abuf insertAll(3, sq)
buf -= 1
buf remove 1
buf
//buf remove(1, 1)
//buf trimStart 1
//buf trimEnd 1
//buf.clone()
//buf.clear()

val fruit = Set("apple", "orange", "peach")
fruit("peach")
fruit("potato")

val set = Set(1,2,3)
set contains 1
set(2)
Set(2,3).subsetOf(set)
set + 4
set + (4,5,6)
set ++ Set(4,5)
set - 1
set - (1,2)
set -- Set(1,2)
set.empty
set & Set(1,2)
set | Set(3,4)
set &~ Set(1,2)

val mset = collection.mutable.Set(1,2,3)
mset += 6
mset += (4,5,6)
mset ++= set
mset add 3
mset -= 1
mset -= (2,3)
mset --= Set(4)
mset remove 0
mset retain (_ < 100)
//mset(0) = 1
mset.clone
mset.clear

val myOrdering = Ordering.fromLessThan[String](_ > _)
import scala.collection.immutable.TreeSet
TreeSet.empty(myOrdering)

val tset = TreeSet.empty[String]
val numbers = tset + ("one", "two", "three", "four")
numbers range("one", "two")
numbers from "three"

val bset = mutable.BitSet(1,2,3)

val map = Map("foo" -> 10, "bar" -> 20, "bazz" -> 30)
map get "foo"
map("foo")
map getOrElse ("fo", 100)
map contains "bazz"
map isDefinedAt "bar"
map + ("quux" -> 40)
map + ("foob" -> 50, "azz" -> 60)
map ++ Map("zaab" -> 70, "xuuq" -> 80)
//map updated("foo" -> 15)
map - "foo"
map - ("bar", "baz")
map -- Seq("foo", "bazz")
map.keys
map.keySet
map.keysIterator
map.values
map.valuesIterator
map.filterKeys(_.length > 3)
map.mapValues(_ * 10)

val mmap = collection.mutable.Map("foo" -> 10, "bar" -> 20, "bazz" -> 30)
//mmap("foo") = 10
mmap += ("quux" -> 50)
mmap += ("foob" -> 140, "baz" -> 150)
mmap ++= collection.mutable.Map("muux" -> 15)
mmap put ("fooba", 1)
mmap getOrElseUpdate("foo", 110)

mmap -= "foo"
mmap -= ("quux","bar")
mmap --= Set("bar", "baz")
//mmap.remove("foo")
mmap.retain((k,v) => k.length > 2)
mmap.transform((k,v) => v*2)
mmap.clear
mmap.clone()

val cache = collection.mutable.Map[String, String]()
def cachedF(s: String, f: String => String) = cache.getOrElseUpdate(s, f(s))

def cachedF2(arg: String, f: String => String) = cache get arg match {
  case Some(result) => result
  case None =>
    val result = f(arg)
    cache(arg) = result
    result
}

val synmap = new mutable.HashMap[String, String] with
  mutable.SynchronizedMap[String, String] {
  override def default(key: String) = "???"
}

val synset = new mutable.HashSet[Int] with
  mutable.SynchronizedSet[Int]

val str = 1 #:: 2 #:: 3 #:: Stream.empty

def fibFrom(a: Int, b: Int): Stream[Int] =
  a #:: fibFrom(b, a + b)

val fibs = fibFrom(1,1).take(7)
fibs.toList

val vec = collection.immutable.Vector.empty
val vec2 = vec :+ 1 :+ 2
val vec3 = 100 +: vec2
vec3(0)

val stack = collection.immutable.Stack.empty
val hasOne = stack.push(1)
stack
hasOne.top
hasOne.pop

val empty = collection.immutable.Queue[Int]()
val has1 = empty.enqueue(1)
val has123 = has1.enqueue(List(2,3))
val (element, has23) = has123.dequeue

1 to 3
5 to 14 by 3
1 until 3

val set2 = collection.immutable.TreeSet.empty[Int]
set2 + 1 + 3 + 3

val bits = collection.immutable.BitSet.empty
val moreBits = bits + 3 + 4 + 4
moreBits(3)

val lmap = collection.immutable.ListMap(1 -> "one", 2 -> "two")
lmap(2)



val abuf2 = collection.mutable.ArrayBuffer.empty[Int]
abuf2 += 1
abuf2 += 10
abuf2.toArray

val lbuf2 = collection.mutable.ListBuffer.empty[Int]
lbuf2 += 1
lbuf2 += 10
lbuf2.toList

val sbuf = new StringBuilder
sbuf += 'a'
sbuf ++= "bcdef"
sbuf.toString()


val queue = new collection.mutable.Queue[String]
queue += "a"
queue ++= List("b", "c")
queue
queue.dequeue
queue


val stack2 = new collection.mutable.Stack[Int]
stack2.push(1)
stack2
stack2.push(2)
stack2
stack2.top
stack2
stack2.pop
stack2

val map2 = collection.mutable.HashMap.empty[Int, String]
map2 += (1 -> "make a web site")
map2 += (3 -> "profit!")
map2(1)
map2 contains 2

val a1 = Array(1,2,3)
val a2 = a1 map (_ * 3)
val a3 = a2 filter (_ % 2 != 0)
a3.reverse

val seq: Seq[Int] = a1
val a4: Array[Int] = seq.toArray
a1 eq a4

val seq2: Seq[Int] = a1
seq2.reverse
val ops: collection.mutable.ArrayOps[Int] = a1
ops.reverse


def evenElems[T: ClassManifest](xs: Vector[T]): Array[T] = {
  val arr = new Array[T]((xs.length + 1) / 2)
  for (i <- 0 until xs.length by 2)
    arr(i / 2) = xs(i)
  arr
}

evenElems(Vector(1,2,3,4,5))
evenElems(Vector("this", "is", "a", "test", "run"))

def wrap[U: ClassManifest](xs: Vector[U]) = evenElems(xs)

val str2 = "hello"
str2.reverse
str2.map(_.toUpper)
str2 drop 3
str2 slice (1,4)
val s: Seq[Char] = str2

//import collection.mutable.{ HashMap, ArrayBuffer }
val buf3 = ArrayBuffer(1,2,3)
val map3 = HashMap(buf3 -> 3)

map3(buf3)
buf3(0) += 1
//map3(buf3)


def lazyMap[T, U](coll: Iterable[T], f: T => U) =
  new Iterable[U] {
    override def iterator = coll.iterator map f
  }

val v = Vector(1 to 10: _*)
(v.view map (_ + 1) map (_ * 2)).force

def isPalindrome(x: String) = x == x.reverse
def findPalindrome(s: Seq[String]) = s find isPalindrome

val words = Seq("foo", "bar", "bazz")
findPalindrome(words.view take 2)

val arr2 = (0 to 9).toArray
val subarr = arr2.view.slice(3,6)
def negate(xs: collection.mutable.Seq[Int]) =
  for (i <- 0 until xs.length) xs(i) = -xs(i)

negate(subarr)
arr2

val it2 = Iterator("a", "number", "of", "words")
val (it3, it4) = it2.duplicate
while (it2.hasNext)
  print(it2.next)
it foreach println
for (elem <- it2) println(elem)
it2.map(_.length)

//it3.next()
it3.hasNext
it3.buffered
//it3 grouped 2
//it3 sliding 2
// ...

val it5 = it4.buffered
it5.head
//it5.next
//it5.next

Traversable()
List()
List(1.0, 2.0)
Vector(1.0, 2.0)
Iterator(1,2,3)
Set(1,2,3)
mutable.HashSet(1,2,3)
Map('a' -> 7, 'b' -> 0)

List.apply(1,.0, 2.0)
mutable.Traversable(1,2,3)

List.empty[Int]
Map.empty[Int, Int]

val S = List(1,2,3)
//S.concat(4,5)
//S.fill(10,5)
//S.fill(2,2)(5)
//S.tabulate(10)(_ => 5)
//S.tabulate(2,2)(_ => 5)
//S.range(1,5)
//S.range(1,5,2)
//S.iterate(5, 10)(_ * 2)

import collection.JavaConversions._

val jul: java.util.List[Int] = ArrayBuffer(1,2,3)
