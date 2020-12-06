val fruit = List("apples", "oranges", "pears")
val nums = List(1,2,3,4)
val diag3 =
  List(
    List(1, 0, 0),
    List(0, 1, 0),
    List(0, 0, 1)
  )
val empty: List[Nothing] = List()

val xs: List[String] = List()

val fruit2 = "apples" :: ("oranges" :: Nil)

//Nil.head

def isort(xs: List[Int]): List[Int] =
  if (xs.isEmpty) Nil
  else insert(xs.head, isort(xs.tail))

def insert(x: Int, xs: List[Int]): List[Int] =
  if (xs.isEmpty || x <= xs.head) x :: xs
  else xs.head :: insert(x, xs.tail)

val List(a, b, c) = fruit
a

isort(List(5,4,1,9,2))

List(1,2) ::: List(3,4,5)

def append[T](xs: List[T], ys: List[T]): List[T] =
  xs match {
    case List() => ys
    case x :: xs1 => x :: append(xs1, ys)
  }

List(1,2,3).length

val abcde = List('a', 'b', 'c', 'd', 'e')
abcde.last
abcde.init

//List().init
//List().last

abcde.reverse
abcde

def rev[T](xs: List[T]): List[T] = xs match {
  case List() => xs
  case x :: xs1 => rev(xs1) ::: List(x)
}

abcde take 2
abcde drop 2
abcde splitAt 2

abcde apply 2
abcde(2)

abcde.indices

List(
  List(1,2),
  List(3),
  List(4,5)
).flatten

fruit.map(_.toCharArray).flatten

abcde.indices zip abcde

val zipped = abcde zip List(1, 2, 3)

abcde.zipWithIndex

zipped.unzip

abcde.toString

abcde mkString ("[", ",", "]")

val buf = new StringBuilder
abcde addString (buf, "(", ";", ")")

val arr = abcde.toArray
arr.toList

val arr2 = new Array[Int](10)
List(1,2,3) copyToArray (arr2, 3)

arr2

val it = abcde.iterator

it.next
it.next

def msort[T](less: (T, T) => Boolean)(xs: List[T]): List[T] = {
  def merge(xs: List[T], ys: List[T]): List[T] =
    (xs, ys) match {
      case (Nil, _) => ys
      case (_, Nil) => xs
      case (x :: xs1, y :: ys1) =>
        if (less(x, y)) x :: merge(xs1, ys)
        else y :: merge(xs, ys1)
    }

  val n = xs.length / 2
  if (n == 0) xs
  else {
    val (ys, zs) = xs splitAt n
    merge(msort(less)(ys), msort(less)(zs))
  }
}

msort((x:Int, y:Int) => x < y)(List(5,7,1,3))

val intSort = msort((x: Int, y: Int) => x < y) _
val reverseTntSort = msort((x: Int, y: Int) => x < y) _

val mixedInts = List(4,1,3,6,8,4,2,1,6,4,2,4)
intSort(mixedInts)

List(1,2,3) map (_ + 1)
val words = List("the", "quick", "brown", "fox")
words map (_.length)
words map (_.toList.reverse.mkString)
words map (_.toList)

words flatMap (_.toList)
List.range(1,5) flatMap (
  i => List.range(1, i) map (j => (i, j))
)

for (i <- List.range(1,5); j <- List.range(1, i)) yield (i, j)

var sum = 0
List(1,2,3,4,5) foreach (sum += _)
sum

List(1,2,3,4,5) filter (_ % 2 == 0)
words filter (_.length == 3)

List(1,2,3,4,5) partition (_ % 2 == 0)

List(1,2,3,4,5) find (_ % 2 == 0)
List(1,2,3,4,5) find (_ <= 0)

List(1,2,3,-4,5) takeWhile (_ > 0)
words dropWhile (_ startsWith "t")

List(1,2,3,-4,5) span (_ > 0)   // equals (xs takeWhile p, xs dropWhile p)

def hasZeroRow(m: List[List[Int]]) =
  m exists (row => row forall (_ == 0))

def sum(xs: List[Int]): Int =
  (0 /: xs)(_ + _)

def product(xs: List[Int]): Int =
  (1 /: xs)(_ * _)

sum(List(2,2,3))
product(List(2,2,3))

(words.head /: words.tail)(_ + " " + _)

def flattenLeft[T](xss: List[List[T]]) =
  (List[T]() /: xss)(_ ::: _)

def flattenRight[T](xss: List[List[T]]) =
  (xss :\ List[T]())(_ ::: _)

def reverseLeft[T](xs: List[T]) =
  (List[T]() /: xs) {
    (ys, y) => y :: ys
  }

reverseLeft(List(1,2,3))

List(1,-3, 4, 2, 6) sortWith (_ < _)
words sortWith (_.length > _.length)

List.apply(1,2,3)

List.range(1, 5)

List.range(1, 9, 2)

List.range(9, 1, -3)

List.fill(5)('a')

List.fill(3)("hello")

List.fill(2, 3)('b')

val squares = List.tabulate(5)(n => n * n)

val multiplication = List.tabulate(5,5)(_ * _)

List.concat(List('a', 'b'), List('c'))

List.concat(List(), List('b'), List('c'))

List.concat()

(List(10,20), List(3,4,5)).zipped.map(_ * _)

(List("abc", "de"), List(3,2)).zipped.exists(_.length != _)

abcde sortWith (_ > _)

//msort(_ > _)(abcde)

msort[Char](_ > _)(abcde)

def msortSwapped[T](xs: List[T])(less: (T,T) => Boolean): List[T] = ???

msortSwapped(abcde)(_ > _)

// (xss :\ List[T]())(_ ::: _)