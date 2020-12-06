abstract class Iterator[A] {
  def first()
  def next()
  def isDone: Boolean
  def currentItem: A
}

class ListIterator[A](list: ListAggregate[A]) extends Iterator[A] {
  var current: Int = 0

  override def first(): Unit = current = 0
  override def next(): Unit = current += 1
  override def isDone: Boolean = current >= list.count

  override def currentItem: A = {
    if (isDone) throw new Exception("Iterator out of bounds")
    list.get(current)
  }
}

abstract class Aggregate[A] {
  def createIterator: Iterator[A]
}

class ListAggregate[A]() extends Aggregate[A] {
  val list = collection.mutable.ListBuffer[A]()

  def count = list.size
  def get(idx: Int) = list(idx)
  def add(v: A): Unit = list.addOne(v)

  override def createIterator: Iterator[A] = new ListIterator[A](this)
}

type Employee = String
val employees = new ListAggregate[Employee]()
employees.add("Jon")
employees.add("Sam")
employees.add("Lucy")

val it = employees.createIterator
it.first()
while (!it.isDone) {
  println(it.currentItem)
  it.next()
}

