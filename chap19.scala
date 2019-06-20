class SlowAppendQueue[T](elems: List[T]) {
  def head = elems.head
  def tail = new SlowAppendQueue(elems.tail)
  def enqueue(x: T) = new SlowAppendQueue(elems ::: List(x))
}

class Queue[+T] private (
              private val leading: List[T],
              private val trailing: List[T]
              ) {

//  def this() = this(Nil, Nil)
//  def this(elems: T*) = this(elems.toList, Nil)

  private def mirror =
    if  (leading.isEmpty)
      new Queue(trailing.reverse, Nil)
    else
      this

  def head = mirror.leading.head

  def tail = {
    val q = mirror
    new Queue(q.leading.tail, q.trailing)
  }

  def enqueue[U >: T](x: U) =
    new Queue(leading, x :: trailing)

}

object Queue {
  def apply[T](xs: T*) = new Queue[T](xs.toList, Nil)
}

//new Queue(1,2,3)
Queue(1,2,3)

trait Queue2[T] {
  def head: T
  def tail: Queue2[T]
  def enqueue(x: T): Queue2[T]
}

object Queue2 {

  def apply[T](xs: T*): Queue2[T] =
    new Queue2Impl[T](xs.toList, Nil)

  private class Queue2Impl[T](
                             private val leading: List[T],
                             private val trailing: List[T]
                             ) extends Queue2[T] {

    def mirror =
      if (leading.isEmpty)
        new Queue2Impl(trailing.reverse, Nil)
      else
        this

    def head: T = mirror.leading.head

    def tail: Queue2Impl[T] = {
      val q = mirror
      new Queue2Impl(q.leading.tail, q.trailing)
    }

    def enqueue(x: T) = new Queue2Impl(leading, x :: trailing)

  }

}

//def doesNotCompile(q: Queue) {}

def doesCompile(q: Queue[AnyRef]) {}

trait Queue3[+T] {}   // subtype of Queue[AnyRef]

class Cell[T](init: T) {
  private [this] var current = init
  def get = current
  def set(x: T) { current = x }
}

//val c1 = new Cell[String]("abc")
//val c2: Cell[Any] = c1
//c2.set(1)
//val s: String = c1.get

//class StrangeIntQueue extends Queue[Int] {
//  override def enqueue(x: Int) = {
//    println(math.sqrt(x))
//    super.enqueue(x)
//  }
//}

//val x: Queue[Any] = new StrangeIntQueue
//x.enqueue("abc")

trait OutputChannel[-T] {
  def write(x: T)
}

trait Function1[-S, +T] {
  def apply(x: S): T
}


class Publication(val title: String)
class Book(title: String) extends Publication(title)

object Library {
  val books: Set[Book] =
    Set(
      new Book("Programming in Scala"),
      new Book("Walden")
    )

  def printBookList(info: Book => AnyRef): Unit = {
    for (book <- books) println(info(book))
  }
}

def getTitle(p: Publication): String = p.title
Library.printBookList(getTitle)


class Queue4[+T] private (
                        private[this] var leading: List[T],
                        private[this] var trailing: List[T]
                        ) {
  private def mirror() =
    if (leading.isEmpty) {
      while (trailing.nonEmpty) {
        leading = trailing.head :: leading
        trailing = trailing.tail
      }
    }

  def head: T = {
    mirror()
    leading.head
  }

  def tail: Queue4[T] = {
    mirror()
    new Queue4(leading.tail, trailing)
  }

  def enqueue[U >: T](x: U) =
    new Queue4[U](leading, x :: trailing)
}


class Person(val firstName: String, val lastName: String) extends Ordered[Person] {
  def compare(that: Person) = {
    val lastNameComparison =
      lastName.compareToIgnoreCase(that.lastName)
    if (lastNameComparison != 0)
      lastNameComparison
    else
      firstName.compareToIgnoreCase(that.firstName)
  }

  override def toString: String = firstName + " " + lastName
}

val robert = new Person("Robert", "Jones")
val sally = new Person("Sally", "Smith")
robert < sally

def orderedMergeSort[T <: Ordered[T]](xs: List[T]): List[T] = {
  def merge(xs: List[T], ys: List[T]): List[T] =
    (xs, ys) match {
      case (Nil, _) => ys
      case (_, Nil) => xs
      case (x :: xs1, y :: ys1) =>
        if (x < y) x :: merge(xs1, ys)
        else y :: merge(xs, ys1)
    }

  val n = xs.length / 2
  if (n == 0) xs
  else {
    val (ys, zs) = xs splitAt n
    merge(orderedMergeSort(ys), orderedMergeSort(zs))
  }
}

val people = List(
  new Person("Larry", "Wall"),
  new Person("Anders", "Hejilsberg")
)

val sortedPeople = orderedMergeSort(people)

//val wontCompile = orderedMergeSort(List(3, 2, 1))
