import scala.annotation.tailrec

object MyModule {
  def abs(n: Int) = if (n < 0) -n else n
  def main(args: Array[String]) = println(abs(-42))
}

MyModule.main(Array.empty[String])

def fact(n: Int) = {
  @tailrec
  def go(n: Int, acc: Int): Int = if (n <= 0) acc else go(n-1, n*acc)
  go(n, 1)
}

val lessThan = new Function2[Int, Int, Boolean] {
  override def apply(v1: Int, v2: Int) = v1 < v2
}

def isSorted[A](as: Array[A], gt: (A, A) => Boolean) = {
  def go(n: Int): Boolean = {
    if (n >= as.length-1) true
    else if (gt(as(n+1), as(n))) false
    else go(n + 1)
  }
  go(0)
}

val b = lessThan.apply(10, 20)
val c = lessThan(10, 20)

isSorted(Array(1,2,3), (a:Int,b:Int) => a < b)
isSorted(Array(4,2,3), (a:Int,b:Int) => a < b)


def partial[A,B,C](a: A, f: (A,B) => C): B => C = b => f(a,b)
def curry[A,B,C](f: (A,B) => C): A => (B => C) = a => b => f(a,b)
def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))

val f = (x:Double) => math.Pi / 2 - x
val cos = f.andThen(math.sin)