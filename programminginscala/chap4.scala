//class ChecksumAccumulator

//val ca = new ChecksumAccumulator
//ca

class ChecksumAccumulator {
  private var sum = 0

  def add(b: Byte): Unit = sum += b
  def checksum(): Int = ~(sum & 0xff) + 1
}

object ChecksumAccumulator {
  private val cache = scala.collection.mutable.Map[String, Int]()

  def calculate(s: String): Int =
    if (cache.contains(s))
      cache(s)
    else {
      val acc = new ChecksumAccumulator
      for (c <- s)
        acc.add(c.toByte)
      val cs = acc.checksum()
      cache += (s -> cs)
      cs
    }
}

val acc = new ChecksumAccumulator
val csa = new ChecksumAccumulator

def g() {
  "this string gets lost too"
}


g()

ChecksumAccumulator.calculate("Every value is an object.")

// A Scala application

import ChecksumAccumulator.calculate

object Summer {
  def main(args: Array[String]): Unit = {
    for (arg <- args) {
      println(s"$arg : ${calculate(arg)}")
    }
  }
}

object FallWinterSpringSummer extends Application {
  for (season <- List("fall", "winter", "spring"))
    println(s"${season}: ${calculate(season)}")
}