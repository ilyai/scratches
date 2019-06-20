val colors = List("red", "blue", "green")

colors.head

colors.tail

val fiveInts = new Array[Int](5)

val fiveToOne = Array(5,4,3,2,1)

fiveInts(0) = fiveToOne(4)

fiveInts

import scala.collection.mutable.ListBuffer

val buf = new ListBuffer[Int]

buf += 1

buf += 2

buf

3 +=: buf

buf.toList

import scala.collection.mutable.ArrayBuffer

val abuf = new ArrayBuffer[Int]()

abuf += 12

abuf += 15

abuf

abuf.length

abuf(0)

def hasUpperCase(s: String) = s.exists(_.isUpper)

hasUpperCase("Robert Frost")

hasUpperCase("e e cummings")

import scala.collection.mutable

val mutaSet = mutable.Set(1,2,3)

val text = "See Spot run. Run, Spot. Run!"

val wordsArray = text.split("[ !,.]+")

val words = mutable.Set.empty[String]

for (word <- wordsArray) {
  words += word.toLowerCase
}

val map = mutable.Map.empty[String, Int]

val nums = Set(1,2,3)

nums + 5

nums - 3

nums ++ List(5,6)

nums -- List(1,2)

nums & Set(1,3,5,7)

nums.size

nums.contains(3)

words += "the"

words -= "the"

words ++= List("do", "re", "mi")

words --= List("do", "re")

words.clear()

//map("hello") = 1
//
//map("there") = 2

def countWords(text: String) = {
  val counts = mutable.Map.empty[String, Int]
  for (rawWord <- text.split("[ ,!.]+")) {
    val word = rawWord.toLowerCase
    val oldCount =
      if (counts.contains(word)) counts(word)
      else 0
    counts += (word -> (oldCount + 1))
  }
  counts
}

countWords(text)


val nums2 = Map("i" -> 1, "ii" -> 2)

nums2 + ("vi" -> 6)
nums2 - "ii"

nums2 ++ List("iii" -> 3, "v" -> 5)
nums2 -- List("i", "ii")
nums2.size
nums2.contains("ii")
nums2.keys
nums2.keySet
nums2.values
nums2.isEmpty

val words2 = mutable.Map.empty[String, Int]
words2 += ("one" -> 1)
words2 -= "one"
words2 ++= List("one" -> 1, "two" -> 2, "three" -> 3)
words2 --= List("one", "two")

import scala.collection.immutable.TreeSet
val ts = TreeSet(9,3,1,8,0,2,8,4,6,5)
val cs = TreeSet('f', 'u', 'n')

import scala.collection.immutable.TreeMap
var tm = TreeMap(3 -> 'x', 1 -> 'x', 4 -> 'x')
tm += (2 -> 'x')

tm

var people = Set("Nancy", "Jane")
people += "Bob"

var roughlyPi = 3.0
roughlyPi += 0.1
roughlyPi += 0.04
roughlyPi

List(1,2,3)
Set('c', 'b', 'c')

mutable.Map("hi" -> 2, "there" -> 5)
Array(1.0, 2.0, 3.0)

val stuff = mutable.Set[Any](42)
stuff += "abracadabra"

//val treeSet = TreeSet(colors)
val treeSet = TreeSet[String]() ++ colors

treeSet.toList
treeSet.toArray

val mutaSet2 = mutable.Set.empty ++ treeSet
val immutaSet = Set.empty ++ mutaSet2

val muta3 = mutable.Map("i" -> 1, "ii" -> 2)
val immu = Map.empty ++ muta3


(1, "hello", Console)

def longestWord(words: Array[String]) = {
  var word = words(0)
  var idx = 0
  for (i <- 1 until words.length) {
    if (words(i).length > word.length) {
      word = words(i)
      idx = i
    }
  }
  (word, idx)
}

val (word, idx) = longestWord("The quick brown fox".split(" "))