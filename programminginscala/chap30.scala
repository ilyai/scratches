//class Any {
//  final def == (that: Any): Boolean =
//    if (null eq this) {null eq that} else {this equals that}
//}

//var hashSet: Set[C] = new collection.immutable.HashSet
//hashSet += elem1
//hashSet contains elem2

class Point(val x: Int, val y: Int) {
  override def equals(other: Any): Boolean = other match {
    case that: Point =>
      (that canEqual this) &&
        (this.x == that.x) && (this.y == that.y)
    case _ => false
  }
  def canEqual(other: Any) = other.isInstanceOf[Point]
  override def hashCode = 41 * (41 + x) + y
}

val p1, p2 = new Point(1, 2)

val q = new Point(2,3)

p1 equals p2
p1 equals q

import scala.collection.mutable._
val coll = HashSet(p1)
coll contains p2

val p2a: Any = p2

p1 equals p2a

HashSet(p1) contains p2

object Color extends Enumeration {
  val Red, Orange, Yellow, Green, Blue, Indigo, Violet = Value
}

class ColoredPoint(x: Int, y: Int, val color: Color.Value) extends Point(x,y) {
  override def hashCode = 41 * super.hashCode + color.hashCode
  override def equals(other: Any): Boolean = other match {
//    case that: ColoredPoint =>
//      this.color == that.color && super.equals(that)
//    case that: Point =>
//      (that equals this) &&
//        (this.x == that.x) && (this.y == that.y)
    case that: ColoredPoint =>
      (that canEqual this) &&
      super.equals(that) && this.color == that.color
    case _ => false
  }
  override def canEqual(other: Any) = other.isInstanceOf[ColoredPoint]
}

val p = new Point(1,2)
val cp = new ColoredPoint(1,2, Color.Red)
p equals cp
cp equals p

HashSet[Point](p) contains cp
HashSet[Point](cp) contains p

val redp = new ColoredPoint(1, 2, Color.Red)
val bluep = new ColoredPoint(1, 2, Color.Blue)

redp == p
p == bluep

val pAnon = new Point(1, 1) { override val y = 2 }

val p3 = new Point(1, 2)
val cp2 = new ColoredPoint(1, 2, Color.Indigo)
val pAnon2 = new Point(1, 1) {
  override val y = 2
}


val coll2 = List(p)
coll2 contains p
coll2 contains cp2
coll2 contains pAnon2

trait Tree[+T] {
  def elem: T
  def left: Tree[T]
  def right: Tree[T]
}

object EmptyTree extends Tree[Nothing] {
  def elem = throw new NoSuchElementException("EmptyTree.elem")
  def left = throw new NoSuchElementException("EmptyTree.left")
  def right = throw new NoSuchElementException("EmptyTree.right")
}

class Branch[+T](
                val elem: T,
                val left: Tree[T],
                val right: Tree[T]
                ) extends Tree[T] {
  override def equals(other: scala.Any): Boolean = other match {
    case that: Branch[_] =>
      this.elem == that.elem &&
      this.left == that.left &&
      this.right == that.right
    case _ => false
  }

  override def hashCode(): Int =
    41 * (
      41 * (
        41 + elem.hashCode()
      ) + left.hashCode()
    ) + right.hashCode()

  def canEqual(other: Any) = other match {
    case that: Branch[_] => true
    case _ => false
  }

//  def canEqual(other: Any) = other.isInstanceOf[Branch[_]]
}

val b1 = new Branch[List[String]](Nil, EmptyTree, EmptyTree)
val b2 = new Branch[List[Int]](Nil, EmptyTree, EmptyTree)

b1 == b2


class Rational(n: Int, d: Int) {
  require(d != 0)

  private val g = gcd(n.abs, d.abs)
  val numer = (if (d < 0) -n else n) / g
  val denom = d.abs / g

  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)

  override def equals(other: scala.Any): Boolean =
    other match {
      case that: Rational =>
        (that canEqual this) &&
        numer == that.numer &&
        denom == that.denom
      case _ => false
    }

  def canEqual(other: Any): Boolean =
    other.isInstanceOf[Rational]

  override def hashCode(): Int =
    41 * (
      41 + numer
    ) + denom

  override def toString =
    if (denom == 1) numer.toString
    else s"$numer/$denom"
}




