import java.io.FileNotFoundException

val args = Array("foo")

val filename = if (!args.isEmpty) args(0) else "default.txt"

def readLine() = ""

var line = ""
do {
  line = readLine()
  println(s"Read: ${line}")
} while (line != "")

def greet(): Unit = {
  println("Hi")
}

greet() == ()

def gcd(x: Long, y: Long): Long =
  if (y == 0) x else gcd(y, x % y)

val filesHere = (new java.io.File(".")).listFiles

for (file <- filesHere if file.getName.endsWith(".xml"))
  println(file)

for (
  file <- filesHere
  if file.isFile
  if file.getName.endsWith(".sh")
) println(file)

for (i <- 1 until 4) yield i

def fileLines(file: java.io.File) =
  scala.io.Source.fromFile(file).getLines().toList

def grep(pattern: String) =
  for {
    file <- filesHere
    if file.getName.endsWith(".scala")
    line <- fileLines(file)
    if line.trim.matches(pattern)
  } println(s"$file: ${line.trim}")

val n = 2

val half =
  if (n % 2 == 0)
    n / 2
  else
    throw new RuntimeException("n must be even")

try {
  val f = new java.io.FileReader("input.txt")
} catch {
  case ex: FileNotFoundException =>
    println("not found")
  case ex: Exception =>
} finally {
  //
}

def f(): Int = try {
  1
} finally {
  2
}

val firstArg = if (args.length > 0) args(0) else ""

firstArg match {
  case "salt" => println("pepper")
  case _ => println("huh?")
}

def searchFrom(i :Int): Int =
  if (i >= args.length) -1
  else if (args(i).startsWith("-")) searchFrom(i + 1)
  else if (args(i).endsWith(".scala")) i
  else searchFrom(i + 1)

val i = searchFrom(0)

import scala.util.control.Breaks._
import java.io._

val in = new BufferedReader(new InputStreamReader(System.in))

//breakable {
//  while (true) {
//    println("? ")
//    if (in.readLine() == "") break
//    break
//  }
//}


val a = 1

{
  val a = 2
}

def makeRowSeq(row: Int) =
  for (col <- 1 to 10) yield {
    val prod = (row * col).toString
    val padding = " " * (4 - prod.length)
    padding + prod
  }

