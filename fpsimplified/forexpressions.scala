import scala.collection.mutable.ArrayBuffer

case class Person(firstName: String, lastName: String)

val people = List(
  Person("barney", "rubble"),
  Person("fred", "flintstone")
)

val namesStartingWithB = for {
  p <- people
  name = p.firstName
  if name.startsWith("b")
} yield name.toUpperCase

abstract class CustomClass[A] {
  def map[B](f: A => B): B
  def flatMap[B](f: A => CustomClass[B]): CustomClass[B]
  def withFilter(p: A => Boolean): CustomClass[A]
  def foreach(f: A => Unit): Unit
}

case class Sequence[A](initialElements: A*) {
  private val elems = collection.mutable.ArrayBuffer[A]()

  elems ++= initialElements

  def foreach(f: A => Unit): Unit =
    elems.foreach(f)

  def map[B](f: A => B): Sequence[B] = {
    val mappedElems = elems.map(f)
    Sequence(mappedElems.toSeq: _*)
  }

  def flatMap[B](f: A => Sequence[B]): Sequence[B] = {
    val mapRes = map(f)
    flattenLike(mapRes)
  }

  def flattenLike[B](seqOfSeq: Sequence[Sequence[B]]): Sequence[B] = {
    var xs = collection.mutable.ArrayBuffer[B]()
    for (seq <- seqOfSeq) {
      for (e <- seq) {
        xs += e
      }
    }
    Sequence(xs.toSeq: _*)
  }

  def withFilter(p: A => Boolean): Sequence[A] = {
    val filteredElems = elems.filter(p)
    Sequence(filteredElems.toSeq: _*)
  }
}

val strings = Sequence("a", "b", "c")
val nums = Sequence(1,2,3)

for (i <- nums) println(i)
for (i <- nums if i > 2) yield i*2

val myFriends = Sequence(
  Person("Adam", ""),
  Person("David", ""),
  Person("Frank", "")
)

val adamFriends = Sequence(
  Person("Nick", ""),
  Person("David", ""),
  Person("Frank", "")
)

val mutualFriends = for {
  myFriend <- myFriends
  adamsFriend <- adamFriends
  if myFriend.firstName == adamsFriend.firstName
} yield myFriend

mutualFriends.foreach(println)

