object LongLines {
  def processFile(filename: String, width: Int): Unit = {
    def processLine(filename: String, width: Int, line: String): Unit = {
      if (line.length > width)
        println(s"$filename: ${line.trim}")
    }
    val source = scala.io.Source.fromFile(filename)
    for (line <- source.getLines())
      processLine(filename, width, line)
  }
}

var increase = (x: Int) => x + 1

increase(10)

val l = List.tabulate(5)(_ * 2).map(_ - 5).filter(_ > 0)

val f = (_: Int) + (_: Int)

f(2,2)

def sum(a: Int, b: Int, c: Int) = a + b + c

val a = sum _

a(1,2,3)

val b  = sum(1, _: Int, 3)

b(3)

var more = 1
val addMore = (x: Int) => x + more
addMore(10)
more = 99999
addMore(10)

var sum2 = 0
l foreach (sum2 += _)
sum2

def echo(args: String*) =
  for (arg <- args) println(arg)

val arguments = Seq("hello", "world")
echo(arguments: _*)

sum(b = 3, c = 2, a = 1)

def printTime(out: java.io.PrintStream = Console.out) =
  out.println(s"time = ${System.currentTimeMillis()}")

printTime()
printTime(out = Console.err)

def bang(x: Int): Int =
  if (x == 0) throw new Exception("bang!")
  else bang(x - 1)

bang(5)
