import scala.annotation.tailrec

def factorial(n: Int): Int = {
  @tailrec
  def go(n: Int, acc: Int): Int = if (n <= 0) acc else go(n-1, acc*n)
  go(n, 1)
}

factorial(5)

def fib(n: Int): Int = {
  @tailrec
  def go(a: Int, b: Int, n: Int): Int = if (n <= 0) a else go(b, a+b, n-1)
  go(0, 1, n)
}

fib(10)

def formatResult(name: String, n: Int, f: Int => Int) =
  "The %s of %d is %d".format(name, n, f(n))

formatResult("absolute value", -42, math.abs)
formatResult("factorial", 7, factorial)
formatResult("increment", 7, _ + 1)

new Function2[Int, Int, Boolean] {
  override def apply(a: Int, b: Int): Boolean = a < b
}.apply(10, 20)

def binarySearch[A](as: Array[A], key: A)(gt: (A, A) => Boolean): Int = {
  @tailrec
  def go(low: Int, high: Int): Int = {
    if (low > high) -1
    else {
      val mid = (low+high) / 2
      assert(mid == low + (high-low) / 2)
      val d = as(mid)
      if (d == key) mid
      else if (gt(d, key)) go(low, mid-1)
      else go(mid+1, high)
    }
  }
  go(0, as.length-1)
}

binarySearch(Array(1,4,5,90,210), 90)(_ > _)

def isSorted[@specialized A](as: Array[A])(gt: (A,A) => Boolean): Boolean = {
  @tailrec
  def go(n: Int): Boolean =
      if (n <= 1) true
      else if (gt(as(n-1),as(n))) false else go(n-1)
  go(as.length-1)
}

isSorted(Array(1,4,5,90,210))(_ > _)

def partial1[A,B,C](a: A, f: (A,B) => C): B => C = b => f(a,b)

partial1(1, (a:Int,b:Int) => a + b)(2)

def curry[A,B,C](f: (A,B) => C): A => B => C = a => b => f(a,b)

curry((a:Int,b:Int) => a + b)(1)(2)

def uncurry[A,B,C](f: A => B => C): (A,B) => C = (a,b) => f(a)(b)

uncurry((a:Int) => (b:Int) => a + b)(1,2)

def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))

compose((b:Int) => b+1, (a:Int) => a*2)(10)

(((b:Int) => b+1) compose ((a:Int) => a*2))(10)
(((b:Int) => b+1) andThen ((a:Int) => a*2))(10)

val f = (x:Double) => math.Pi / 2 - x
val cos = f andThen math.sin

cos(10)