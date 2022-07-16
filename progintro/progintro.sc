def divisorPattern(n: Int) =
  for (i <- 1 to n) {
    for (j <- 1 to n) {
      print(if ((i % j == 0) || (j % i) == 0) "* " else "  ")
    }
    println(i)
  }

//divisorPattern(16)

def harmonic(n: Int) =
  (1 to n).foldLeft(0.0)((sum,i) => sum + 1.0/i)

//harmonic(2)
//harmonic(10)
//harmonic(10000)

// p.62
def sqrt(c: Double): Double = {
  val eps = 1e-15
  def go(t: Double): Double =
    if (Math.abs(t - c/t) > eps * t)
      go((c/t + t) / 2.0)
    else t
  go(c)
}

sqrt(2.0)
sqrt(2544545)

def toBinaryString(n: Int): String = {
  def go(n: Int, s: String): String =
    if (n > 0) go(n/2, (n % 2).toString ++ s)
    else s
  go(n, "")
}

toBinaryString(19)
toBinaryString(255)

def ruler: String = {
  val ruler1 = "1"
  val ruler2 = ruler1 + " 2 " + ruler1
  val ruler3 = ruler2 + " 3 " + ruler2
  val ruler4 = ruler3 + " 4 " + ruler3
  ruler4
}

ruler

def randomInt(n: Int): Int = (math.random() * n).toInt
randomInt(100)
randomInt(1000000)

def powersOfTwo(n: Int) = {
  def go(i: Int, v: Int): Unit = {
    if (i > n) return
    println(i + " " + v)
    go(i + 1, v * 2)
  }
  go(0, 1)
}

powersOfTwo(5)

def gambler(stake: Int, goal: Int, t: Int): Unit = {
  def go(bets: Int, wins: Int, cash: Int, c: Int): (Int,Int,Int) = {
    if (c >= t) {
      (bets,wins,c)
    } else if (cash > 0 && cash < goal) {
      val cashNow = if (math.random() < 0.5) cash+1 else cash-1
      go(bets+1, wins, cashNow, c)
    } else if (c < t) {
      go(bets, if (cash == goal) wins+1 else wins, stake, c+1)
    } else {
      (bets,wins,c)
    }
  }
  val (bets,wins,count) = go(0, 0, stake, 0)
  println(100*wins/count + "% wins")
  println("Avg # bets: " + bets/count)
}
gambler(10, 20, 1000)
gambler(50, 250, 100)