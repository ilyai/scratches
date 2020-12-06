case class Person(name: String,
                  isMale: Boolean,
                  children: Person*)

val lara = Person("Lara", false)
val bob = Person("Bob", true)
val julie = Person("Julie", false, lara, bob)
val persons = List(lara, bob, julie)

persons filter (p => !p.isMale) flatMap (p =>
  (p.children map (c => (p.name, c.name))))

for {
  p <- persons
  if !p.isMale
  c <- p.children
} yield (p.name, c.name)

for (p <- persons; n = p.name; if (n startsWith "To"))
  yield n

for (x <- List(1,2); y <- List("one", "two"))
  yield (x, y)


def queens(n: Int): List[List[(Int, Int)]] = {
  def isSafe(queen: (Int, Int), queens: List[(Int, Int)]) =
    queens forall (q => !inCheck(queen, q))

  def inCheck(q1: (Int, Int), q2: (Int, Int)): Boolean =
    q1._1 == q2._1 ||   // same row
    q1._2 == q2._2 ||   // same column
      (q1._1 - q2._1).abs == (q1._2 - q2._2).abs

  def placeQueens(k: Int): List[List[(Int, Int)]] =
    if (k == 0)
      List(List())
    else
      for {
        queens <- placeQueens(k - 1)
        column <- 1 to n
        queen = (k, column)
        if isSafe(queen, queens)
      } yield queen :: queens
  placeQueens(n)
}

queens(8)

case class Book(title: String, authors: String*)

val books: List[Book] =
  List(
    Book(
      "Structure and Interpretation of Computer Programs",
      "Abelson, Harold", "Sussman, Gerald J."
    ),
    Book(
      "Principles of Compiler Design",
      "Aho, Alfred", "Ullman, Jeffrey"
    )
  )

for (b <- books; a <- b.authors if a startsWith "Ullman")
  yield b.title

val r = for {
  b1 <- books
  b2 <- books if b1 != b2
  a1 <- b1.authors
  a2 <- b2.authors if a1 != a2
} yield a1

val r2 = books flatMap (b1 =>
  books withFilter (b2 => b1 != b2) flatMap (b2 =>
    b1.authors flatMap (a1 =>
      b2.authors withFilter (a2 => a1 != a2) map (a2 =>
        a1))
    ))

def removeDuplicates[A](xs: List[A]): List[A] = {
  if (xs.isEmpty) xs
  else
    xs.head :: removeDuplicates(
      xs.tail filter (x => x != xs.head)
    )
}

removeDuplicates(r)

r.head :: removeDuplicates(
  for (x <- r.tail if x != r.head) yield x
)


for ((x,y) <- Some((1,2))) yield x

def expensiveComputationNotInvolvingX = 1
for (x <- 1 to 1000; y = expensiveComputationNotInvolvingX)   // FIXME
  yield x * y

var sum = 0
for (x <- 1 to 10) sum += x
sum

object Demo {
  def map[A, B](xs: List[A], f: A => B): List[B] =
    for (x <- xs) yield f(x)
  def flatMap[A,B](xs: List[A], f: A => List[B]): List[B] =
    for {
      x <- xs
      y <- f(x)
    } yield y
  def filter[A](xs: List[A], p: A => Boolean): List[A] =
    for (x <- xs if p(x)) yield x
}

abstract class C[A] {
  def map[B](f: A => B): C[B]
  def flatMap[B](f: A => C[B]): C[B]
  def withFilter(p: A => Boolean): C[A]
  def foreach(b: A => Unit): Unit
}