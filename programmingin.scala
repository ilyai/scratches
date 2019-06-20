import java.math.BigInteger

import scala.collection.immutable.HashMap

var capitals = Map("US" -> "Washington")

capitals += ("Japan" -> "Tokyo")
capitals

def fact(x: BigInt): BigInt =
  if (x == 0) 1 else fact(x - 1) * x

//fact(30)

class MyClass(index: Int, name: String)

val x1 = new HashMap[Int,String]()
val x2: Map[Int, String] = new HashMap()


val msg3: String = "Hello"
val greeting = "Hello"

def max(x: Int, y: Int) = if (x > y) x else y

def greet() = println(greeting)


var i = 0
val args = Array(1,2,3)
while (i < args.length) {
  println(args(i))
  i += 1
}

args.foreach(arg => println(arg))
for (arg <- args) println(arg)

val big = new BigInteger("12345")

val greetStrings = new Array[String](3)
greetStrings(0) = "foo"
greetStrings(1) = "bar"
greetStrings(2) = "baz"

for (i <- 0 to 2) println(greetStrings(i))

Console println 10

val numNames = Array("zero", "one", "two")
val numNames2 = Array.apply("zero", "one", "two")

val l = List(1,2) ::: List(3, 4)
val l2 = 1 :: 2 :: 3 :: Nil

l.init
l.last

val pair = (99, "L")
pair._1

var jetSet = Set("Boeing", "Airbus")
jetSet += "Lear"

val movieSet = collection.mutable.Set("Hitch", "Poltergeist")
movieSet += "Shrek"
println(movieSet)

val hashSet = collection.immutable.HashSet("Tomatoes", "Chilies")

val treasureMap = collection.mutable.Map[Int, String]()
treasureMap += (1 -> "Go to island")

val romanNum = Map(1 -> "I", 2 -> "II", 3 -> "III")
romanNum(2)

val lines = scala.io.Source.fromFile("/etc/passwd").getLines().toList

lines.reduceLeft((a,b) => if (a.length > b.length) a else b)

"foo" * 5

