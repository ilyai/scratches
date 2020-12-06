/**
  * When to use traits:
  * If behaviour will be reused
  * if it might be reused in multiple, unrelated classes
  * If you don't want to inherit from it in Java code
  * If you don't plan to distribute it in compile form
  * If efficiency is not very important
  */

trait Philosophical {
  def philosophize(): Unit = {
    println("I consume memory, therefore I am!")
    print(super.hashCode())

  }
}

class Frog extends Philosophical {
  override def toString = "green"

  override def philosophize(): Unit = {
    println(s"It ain't easy being $toString")
  }
}

val frog = new Frog
frog.philosophize()

class Animal
trait HasLegs

class Frog2 extends Animal with Philosophical {
  override def toString: String = "green"
}

class Frog3 extends Animal with Philosophical with HasLegs {
  override def toString: String = "green"
}


val phrog: Philosophical = new Frog
phrog.philosophize()

//trait NoPoint(x: Int)   // doesn't compile

trait CharSequence {
  def charAt(index: Int): Char
  def length: Int
  def subSequence(start: Int, end: Int): CharSequence
  def toString: String
}

class Point(val x: Int, val y: Int)

class Rectangle(val topLeft: Point, val bottomRight: Point) extends Rectangular{
//  def left = topLeft.x
//  def right = bottomRight.x
//  def width = right - left
}

abstract class Component extends Rectangular{
  def topLeft: Point
  def bottomRight: Point
//  def left = topLeft.x
//  def right = bottomRight.x
//  def width = right - left
}

trait Rectangular {
  def topLeft: Point
  def bottomRight: Point
  def left = topLeft.x
  def right = bottomRight.x
  def width = right - left
}

val rect = new Rectangle(new Point(1,1), new Point(10,10))

class Rational(n: Int, d: Int) extends Ordered[Rational] {
  def compare(that: Rational) = ???
}

abstract class IntQueue {
  def get(): Int
  def put(x: Int)
}

trait Doubling extends IntQueue {
  abstract override def put(x: Int) = {
    super.put(2 * x)
  }
}

trait Incrementing extends IntQueue {
  abstract override def put(x: Int) = {
    super.put(x + 1)
  }
}

trait Filtering extends IntQueue {
  abstract override def put(x: Int): Unit = {
    if (x >= 0) super.put(x)
  }
}

import scala.collection.mutable.ArrayBuffer
class BasicIntQueue extends IntQueue {
  private val buf = new ArrayBuffer[Int]
  def get() = buf.remove(0)
  def put(x: Int): Unit = {
    buf += x
  }
}

val queue = new BasicIntQueue
queue.put(10)
queue.put(20)
queue.get()
queue.get()

class MyQueue extends BasicIntQueue with Doubling
val mq = new MyQueue
mq.put(20)
mq.get()

val mq2 = new BasicIntQueue with Incrementing with Filtering
mq2.put(20)
mq2.put(-20)
mq2.get()

val mq3 = new BasicIntQueue with Incrementing with Doubling
mq3.put(20)
mq3.get()

trait Furry extends Animal
trait FourLegged extends HasLegs
class Cat extends Animal with Furry with FourLegged



