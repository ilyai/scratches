def replicate(n: Int)(act: => Unit) = for (_ <- 1 to n) act

replicate(5) _
replicate(5)(println("scalac"))

val sum: (Int, Int) => Int = _ + _
sum(2,2)
(sum(2,_:Int))(2)
sum.curried(2)(2)

def sum2(a:Int = 1)(b:Int = 1): Int = a + b
sum2()()