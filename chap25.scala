import scala.collection.mutable
val buf = new collection.mutable.ArrayBuffer[Int]
val bldr = buf.mapResult(_.toArray)

//class TraversableLike[+Elem, +Repr] {
//  def newBuilder: Builder[Elem, Repr]
//  def foreach[U](f: Elem => U)
//  def filter(p: Elem => Boolean): Repr = {
//    val b = newBuilder
//    foreach { elem => if (p(elem)) b += elem }
//    b.result
//  }
//}

import collection.immutable.BitSet
val bits = BitSet(1,2,3)
bits map (_ * 2)
bits map (_.toFloat)

Map("a" -> 1, "b" -> 2) map {
  case (x, y) => (y, x)
}

Map("a" -> 1, "b" -> 2) map {
  case (x, y) => y
}

val xs: Iterable[Int] = List(1,2,3)
val ys = xs map (x => x * x)

abstract class Base
case object A extends Base
case object T extends Base
case object G extends Base
case object U extends Base

object Base {
  val fromInt: Int => Base = Array(A, T, G, U)
  val toInt: Base => Int = Map(A -> 0, T -> 1, G -> 2, U -> 3)
}

import collection.IndexedSeqLike
import collection.mutable.{Builder, ArrayBuffer}
import collection.generic.CanBuildFrom

final class RNA1 private (val groups: Array[Int], val length: Int)
  extends IndexedSeq[Base] {
  import RNA1._

  def apply(idx: Int): Base = {
    if (idx < 0 || length <= idx) throw new IndexOutOfBoundsException
    Base.fromInt(groups(idx / N) >> (idx % N * S) & M)
  }

  override def take(count: Int): RNA1 = RNA1.fromSeq(super.take(count))
}

object RNA1 {
  // Number of bits necessary to represent group
  private val S = 2
  // Number of groups that fin in an Int
  private val N = 32 / S
  // Bitmask to isolate a group
  private val M = (1 << S) - 1

  def fromSeq(buf: Seq[Base]): RNA1 = {
    val groups = new Array[Int]((buf.length + N - 1) / N)
    for (i <- 0 until buf.length)
      groups(i / N) |= Base.toInt(buf(i)) << (i % N * S)
    new RNA1(groups, buf.length)
  }
  def apply(bases: Base*) = fromSeq(bases)
}

val xs2 = List(A, G, T, A)
RNA1.fromSeq(xs2)

val rna1 = RNA1(A, U, G, G, T)
rna1.length
rna1.take(3)

rna1.length
rna1.last
rna1.take(3)

final class RNA2 private (
                         val groups: Array[Int],
                         val length: Int
                         ) extends IndexedSeq[Base] with IndexedSeqLike[Base, RNA2] {
  import RNA2._

  override def newBuilder: mutable.Builder[Base, RNA2] =
    new ArrayBuffer[Base] mapResult fromSeq

  def apply(idx: Int): Base = {
    if (idx < 0 || length <= idx) throw new IndexOutOfBoundsException
    Base.fromInt(groups(idx / N) >> (idx % N * S) & M)
  }
}

object RNA2 {
  // Number of bits necessary to represent group
  private val S = 2
  // Number of groups that fin in an Int
  private val N = 32 / S
  // Bitmask to isolate a group
  private val M = (1 << S) - 1

  def fromSeq(buf: Seq[Base]): RNA2 = {
    val groups = new Array[Int]((buf.length + N - 1) / N)
    for (i <- 0 until buf.length)
      groups(i / N) |= Base.toInt(buf(i)) << (i % N * S)
    new RNA2(groups, buf.length)
  }
  def apply(bases: Base*) = fromSeq(bases)
}

val rna2 = RNA2(A, U, G, G, T)
rna2 take 3
rna2 filter (U != _)

rna2 map { case A => T case b => b }
rna2 ++ rna2


final class RNA private (
                           val groups: Array[Int],
                           val length: Int
                         ) extends IndexedSeq[Base] with IndexedSeqLike[Base, RNA] {
  import RNA._

  override protected[this] def newBuilder: mutable.Builder[Base, RNA] =
    RNA.newBuilder

  def apply(idx: Int): Base = {
    if (idx < 0 || length <= idx) throw new IndexOutOfBoundsException
    Base.fromInt(groups(idx / N) >> (idx % N * S) & M)
  }

  // Optional re-implementation of foreach
  // to make it more efficient
  override def foreach[U](f: Base => U): Unit = {
    var i = 0
    var b = 0
    while (i < length) {
      b = if (i % N == 0) groups(i / N) else b >>> S
      f(Base.fromInt(b & M))
      i += 1
    }
  }
}

object RNA {
  // Number of bits necessary to represent group
  private val S = 2
  // Number of groups that fin in an Int
  private val N = 32 / S
  // Bitmask to isolate a group
  private val M = (1 << S) - 1

  def fromSeq(buf: Seq[Base]): RNA = {
    val groups = new Array[Int]((buf.length + N - 1) / N)
    for (i <- 0 until buf.length)
      groups(i / N) |= Base.toInt(buf(i)) << (i % N * S)
    new RNA(groups, buf.length)
  }

  def apply(bases: Base*) = fromSeq(bases)

  def newBuilder: Builder[Base, RNA] =
    new ArrayBuffer mapResult fromSeq

  implicit def canBuildFrom: CanBuildFrom[RNA, Base, RNA] =
    new CanBuildFrom[RNA, Base, RNA] {
      def apply(): Builder[Base, RNA] = newBuilder
      def apply(from: RNA): Builder[Base, RNA] = newBuilder
    }
}

import collection._

class PrefixMap[T] extends mutable.Map[String, T]
    with mutable.MapLike[String, T, PrefixMap[T]] {
  var suffixes: immutable.Map[Char, PrefixMap[T]] = Map.empty
  var value: Option[T] = None

  def get(s: String): Option[T] =
    if (s.isEmpty) value
    else suffixes get (s(0)) flatMap (_.get(s substring 1))

  def withPrefix(s: String): PrefixMap[T] =
    if (s.isEmpty) this
    else {
      val leading = s(0)
      suffixes get leading match {
        case None =>
          suffixes = suffixes + (leading -> empty)
        case _ =>
      }
      suffixes(leading) withPrefix (s substring 1)
    }

  override def update(s: String, elem: T) =
    withPrefix(s).value = Some(elem)

  override def remove(s: String): Option[T] =
    if (s.isEmpty) {
      val prev = value
      value = None
      prev
    } else {
      suffixes get (s(0)) flatMap (_.remove(s substring 1))
    }

  def iterator: Iterator[(String, T)] =
    (for (v <- value.iterator) yield ("", v)) ++
      (for {
        (chr, m) <- suffixes.iterator
        (s, v) <- m.iterator
      } yield (chr +: s, v))

  def += (kv: (String, T)): this.type = {
    update(kv._1, kv._2)
    this
  }

  def -= (s: String): this.type = {
    remove(s)
    this
  }

  override def empty = new PrefixMap[T]
}

object PrefixMap extends {
  def empty[T] = new PrefixMap[T]

  def apply[T](kvs: (String, T)*): PrefixMap[T] = {
    val m: PrefixMap[T] = empty
    for (kv <- kvs) m += kv
    m
  }

  def newBuilder[T]: Builder[(String, T), PrefixMap[T]] =
    new mutable.MapBuilder[String, T, PrefixMap[T]](empty)

  implicit def canBuildFrom[T]: CanBuildFrom[PrefixMap[_], (String, T), PrefixMap[T]] =
    new CanBuildFrom[PrefixMap[_], (String, T), PrefixMap[T]] {
      def apply(from: PrefixMap[_]) = newBuilder[T]
      def apply() = newBuilder[T]
    }
}

val pm = PrefixMap("hello" -> 5, "hi" -> 2)
PrefixMap.empty[String]

pm map {
  case (k,v) =>
    (k + "!", "x" * v)
}