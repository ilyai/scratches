val jrng = new java.util.Random
jrng.nextDouble()
jrng.nextDouble()
jrng.nextInt()
jrng.nextInt()

trait Random {
  def nextInt: Int
  def nextBoolean: Boolean
  def nextDouble: Double
}

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  def simple(seed: Long): RNG = new RNG {
    override def nextInt: (Int, RNG) = {
      val seed2 = (seed*0x5deece66dL + 0xbL) & ((1L << 48) - 1)
      ((seed2 >>> 16).asInstanceOf[Int], simple(seed2))
    }
  }

  def randomPair(rng: RNG): ((Int,Int),RNG) = {
    val (i1,rng2) = rng.nextInt
    val (i2,rng3) = rng2.nextInt
    ((i1,i2),rng3)
  }

  def positiveInt(rng: RNG): (Int, RNG) = {
    val (i,rng2) = rng.nextInt
    ((if (i == Int.MinValue) i-1 else i).abs, rng2)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i,r) = rng.nextInt
    (if (i < 0) -(i+1) else i, r)
  }

//  0..10 -> 0..1
  def double(rng: RNG): (Double, RNG) = {
    val (i,rng2) = positiveInt(rng)
//    val d =(if (i == Int.MaxValue) i-1 else i).toDouble / Int.MaxValue
    val d = i / (Int.MaxValue.toDouble + 1)
    (d,rng2)
  }

  def boolean(rng: RNG): (Boolean, RNG) =
    rng.nextInt match {
      case (i,rng2) => (i % 2 == 0, rng2)
    }

  def intDouble(rng: RNG): ((Int,Double),RNG) = {
    val (i,rng2) = rng.nextInt // positiveInt(rng)
    val (d,rng3) = double(rng2)
    ((i,d), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int),RNG) = {
    val (d,rng2) = double(rng)
    val (i,rng3) = rng.nextInt // positiveInt(rng2)
    ((d,i), rng3)
  }

  def double3(rng: RNG): ((Double,Double,Double),RNG) = {
    val (d1,r1) = double(rng)
    val (d2,r2) = double(r1)
    val (d3,r3) = double(r2)
    ((d1,d2,d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count <= 0) (Nil, rng)
    else {
      val (i,r2) = positiveInt(rng)
      ints(count-1)(r2) match {
        case (is,r) => (i :: is, r)
      }
    }
  }

  def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(count: Int, r: RNG, xs: List[Int]): (List[Int], RNG) =
      if (count == 0) (xs, r)
      else {
        val (x, r2) = r.nextInt
        go(count-1, r2, x :: xs)
      }
    go(count, rng, List())
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def positiveMax(n: Int): Rand[Int] = map(int)(_.abs % n)

  def double2: Rand[Double] = map(nonNegativeInt)(
    _ / Int.MaxValue.toDouble + 1
  )

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] =
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a,b), r2)
    }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra,rb)((_,_))

  def randIntDouble: Rand[(Int,Double)] =
    both(int, double)

  def randDoubleInt: Rand[(Double,Int)] =
    both(double,int)

  def _sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f,acc) => map2(f, acc)(_ :: _))

  def ints2(count: Int): Rand[List[Int]] =
    _sequence(List.fill(count)(int))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a,r1) = f(rng)
      g(a)(r1)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }

  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a,b)))
}

import RNG._

val rng = RNG.simple(1)
randomPair(rng)
positiveInt(rng)
double(rng)
intDouble(rng)
doubleInt(rng)
double3(rng)
ints(10)(rng)

double2(rng)
positiveMax(10)(rng)



//type State[S,+A] = S => (A,S)
case class State[S,+A](run: S => (A,S)) {
  def map[B](f: A => B): State[S,B] =
    flatMap(a => State(s => (f(a), s)))
  def map2[B,C](sb: State[S,B])(f: (A,B) => C): State[S,C] =
    flatMap(a => sb.map(b => f(a,b)))
  def flatMap[B](f: A => State[S,B]): State[S,B] = State(s => {
    val (a,s1) = run(s)
    f(a).run(s1)
  })
}

object State {
  type Rand[A] = State[RNG,A]

  def unit[S,A](a: A): State[S,A] = State(s => (a,s))

  def sequenceViaFoldRight[S,A](sas: List[State[S,A]]): State[S, List[A]] =
    sas.foldRight(unit[S, List[A]](List()))((f,acc) => f.map2(acc)(_ :: _))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    def go(s: S, actions: List[State[S,A]], acc: List[A]): (List[A],S) =
      actions match {
        case Nil => (acc.reverse,s)
        case h :: t => {
          val (a,s2) = h.run(s)
          go(s2,t,a :: acc)
        }
      }
    State((s: S) => go(s,sas,List()))
  }

  def sequenceViaFoldLeft[S,A](l: List[State[S,A]]): State[S, List[A]] =
    l.reverse.foldLeft(unit[S,List[A]](List()))((acc,f) => f.map2(acc)(_ :: _))

  def modify[S](f: S => S): State[S,Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S,S] = State(s => (s,s))

  def set[S](s: S): State[S,Unit] = State(_ => ((),s))
}

import State._

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
  def update = (i: Input) => (s: Machine) =>
    (i,s) match {
      case (_, Machine(_,0,_)) => s
      case (Coin, Machine(false,_,_)) => s
      case (Turn, Machine(true,_,_)) => s
      case (Coin, Machine(true,candy,coin)) => Machine(false,candy,coin+1)
      case (Turn, Machine(false,candy,coin)) => Machine(true,candy-1,coin)
    }

  def simulateMachine(inputs: List[Input]) = for {
    _ <- sequence(inputs map (modify[Machine] _ compose update))
//    _ <- sequence(inputs.map(i => (modify[Machine] _).compose(update)(i)))
    s <- get
  } yield (s.coins,s.candies)
}

Candy.simulateMachine(List(Coin,Turn)).run(Machine(false,3,0))