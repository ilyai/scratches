val oneTwoTree = List(1,2,3)
oneTwoTree ::: List(4,5)
0 :: oneTwoTree
1 :: 2 :: 3 :: Nil
oneTwoTree(2)
oneTwoTree.count(_ > 2)
oneTwoTree.count(_ > 1)
oneTwoTree.drop(2)
oneTwoTree.dropRight(2)
oneTwoTree.exists(_ > 1)
oneTwoTree.filter(_ > 1)
oneTwoTree.forall(_ > 1)
oneTwoTree.forall(_ > 0)
oneTwoTree.foreach(print _)
oneTwoTree.foreach(println _)
oneTwoTree.foreach(println(_))
oneTwoTree.foreach(println)
oneTwoTree.head
oneTwoTree.init
oneTwoTree.isEmpty
oneTwoTree.last
oneTwoTree.length
oneTwoTree.map(_+"x")
oneTwoTree.mkString(" ")
oneTwoTree.reverse
oneTwoTree.tail

val pair = (99, "Luft")
pair._1
pair._2

var jetSet = Set("Boe", "Air")
jetSet += "Lea"
jetSet.contains("Ces")

import scala.collection.mutable.Set
val movSet = Set("Hit", "Pol")
movSet += "Shr"
movSet

import scala.collection.mutable.Map
val tm = Map[Int, String]()
tm += (1 -> "T")
tm += (2 -> "F")
tm += (3 -> "D")
tm

tm(2)

import scala.io.Source
val lines = Source.fromFile("/etc/passwd").getLines().toList

var maxWidth = 0
for (line <- lines)
  maxWidth = maxWidth.max(line.length)

maxWidth

lines.reduceLeft((a,b) => if (a.length > b.length) a else b)