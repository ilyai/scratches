val xs = List(1,2,3)
var ys: List[Any] = xs

abstract class List[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]

  def length: Int =
    if (isEmpty) 0 else 1 + tail.length

  def drop(n: Int): List[T] =
    if (isEmpty) Nil
    else if (n <= 0) this
    else tail.drop(n - 1)

//  def ::[U >: T](x: U) = List[U] = new scala.::(x, this)

//  def :::[U >: T](prefix: List[U]): List[U] =
//    if (prefix.isEmpty) this
//    else prefix.head :: prefix.tail ::: this

  def map[U](f: T => U): List[U] =
    if (isEmpty) Nil
    else ::(f(head), tail.map(f))

  final override def map[U](f: T => U): List[U] = {
    val b = new scala.collection.mutable.ListBuffer[U]
    var these = this
    while (!these.isEmpty) {
      b += f(these.head)
      these = these.tail
    }
  }
}

case object Nil extends List[Nothing] {
  override def isEmpty = true
  override def head: Nothing =
    throw new NoSuchElementException("head of empty list")
  override def tail: List[Nothing] =
    throw new NoSuchElementException("tail of empty list")
}

final case class ::[T](hd: T, tl: List[T]) extends List[T] {
  override def head = hd
  override def tail = tl
  override def isEmpty: Boolean = false
}


abstract class Fruit
class Apple extends Fruit
class Orange extends Fruit

//val apples = new Apple :: Nil
//val fruits = new Orange :: apples


//def incAll(xs: List[Int]): List[Int] = xs match {
//  case Nil => List()
//  case x :: xs1 => x + 1 :: incAll(xs1)
//}

var result = List[Int]()
for (x <- xs) result ::: List(x + 1)
result

import scala.collection.mutable.ListBuffer

val buf = new ListBuffer[Int]
for (x <- xs) buf += x + 1
buf.toList


