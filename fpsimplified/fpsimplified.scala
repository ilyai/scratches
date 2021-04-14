val double = (i:Int) => i * 2

val doubleFn = double _

def map[A,B](xs: Seq[A], f: A => B): Seq[B] = for {
  x <- xs
} yield f(x)

map(Seq(1,2,3), (x:Int) => x * x)

def timer[A](blockOfCode: => A) = {
  val startTime = System.nanoTime()
  val result = blockOfCode
  val stopTime = System.nanoTime()
  val delta = stopTime - startTime
  (result, delta/1000000d)
}

timer(println("foobar"))

val (res, time) = timer {
  Thread.sleep(100)
  42
}

val assertionsEnabled = true
def byNameAssert(predicate: => Boolean) =
  if (assertionsEnabled && !predicate)
    throw new AssertionError()

byNameAssert(5 > 3)

def sum(a: Int)(b: Int)(c: Int) = a + b + c

sum(1)(2)(3)

def sum1 = sum(1) _

def sum2 = sum(1)(_: Int)(1)

def sum3(a: Int, b: Int, c: Int) = a + b + c
def sum3curried = (sum3 _).curried

sum1(2)(3)
sum2(3)
sum3curried(1)(2)(3)

def whilst(testCondition : => Boolean)(codeBlock: => Unit) = while (testCondition) codeBlock

var i = 0
whilst (i < 5) {
  println("awesome")
  i += 1
}

def printIntIfTrue(a: Int)(implicit b: Boolean) = if (b) println(a)

//implicit val foo = true
//printIntIfTrue(100)

def sum4(list: List[Int]): Int = list match {
  case Nil => {
    Thread.currentThread().getStackTrace.foreach(println)
    0
  }
  case x :: xs => x + sum4(xs)
}

sum4(1 :: 2 :: 3 :: Nil)

def sum5(list: List[Int]): Int = {
  @scala.annotation.tailrec
  def loop(list: List[Int], acc: Int): Int = list match {
    case Nil => {
      Thread.currentThread().getStackTrace.foreach(println)
      acc
    }
    case x :: xs => loop(xs, x + acc)
  }
  loop(list, 0)
}

sum5(1 :: 2 :: 3 :: Nil)

// apply
// unapply
// accessor and mutator methods
// hashCode
// equals
// copy
// toSting
case class Person(name: String, realtion: String)

val joe = Person("Joe", "father")
val mark = joe.copy(name = "Mark")
joe match { case Person(n,_) => n }

for {
  p <- Seq(joe,mark)      // generator
  n = p.name            // definition
  if (n startsWith "J")     // filter
} yield n

abstract class CustomClass[A] {
  def map[B](f: A => B): CustomClass[B]
  def flatMap[B](f: A => CustomClass[B]): CustomClass[B]
  def withFilter(p: A => Boolean): CustomClass[A]
  def foreach(b: A => Unit): Unit
}

case class Sequence[A](initialElements: A*) {
  private val elems = scala.collection.mutable.ArrayBuffer[A]()

  elems ++= initialElements

  def foreach(block: A => Unit): Unit = elems.foreach(block)

  def map[B](f: A => B): Sequence[B] = {
    val abMap = elems.map(f).toSeq
    Sequence(abMap: _*)
  }

  def flatMap[B](f: A => Sequence[B]): Sequence[B] = flattenLike(map(f))

  private def flattenLike[B](seqOfSeq: Sequence[Sequence[B]]): Sequence[B] = {
    var ab = scala.collection.mutable.ArrayBuffer[B]()
    for (listB: Sequence[B] <- seqOfSeq) {
      for (e <- listB) {
        ab += e
      }
    }

    val xs = ab.toSeq
    Sequence(xs: _*)
  }

  def withFilter(p: A => Boolean): Sequence[A] = Sequence((elems.filter(p).toSeq): _*)
}

val a = Sequence(1,2)
val b = Sequence(1,2,3)
val c = Sequence('a', 'b', 'c')

for (i <- b if i > 1; g <- a) yield i * g

def makeInt(s: String): Option[Int] = try {
  Some(s.trim.toInt)
} catch {
  case _: Exception => None
}

makeInt("foo") match {
  case Some(i) => println(i)
  case None => println("toInt could not parse 'input'")
}

var result = makeInt("foo").getOrElse(1)

var result2 = for {
  x <- makeInt("1")
  y <- makeInt("2")
  z <- makeInt("3")
} yield x + y + z

def makeInt2(s: String) = scala.util.Try(s.trim.toInt)
makeInt2("100")
makeInt2("foo")

def makeInt3(s: String): Either[String,Int] = try {
  Right(s.trim.toInt)
} catch {
  case ex: Exception => Left(ex.toString)
}

trait Woman {
  def name: String
  def age: Int
  override def toString: String = s"name:$name, age:$age"
}

val mary = new Woman {
  val name = "mary"
  val age = 22
}

println(mary)

case class StringToInt(run: String => Int)

val stringToInt = StringToInt { s: String =>
  s.length
}

stringToInt.run("bananas")

def s2i(s: String)(f: String => Int) = f(s)

val result22 = s2i("hello") { s: String =>
  s.length
}

case class s2i2(s: String)(_fun: String => Int) {
  def fun = _fun(s)
}

val r3 = s2i2("hello") { s: String =>
  s.length
}

r3.fun

def using[A <: { def close(): Unit }, B](resource: A)(f: A => B): B = {
  try {
    f(resource)
  } finally {
    resource.close()
  }
}

val x = makeInt("1")
val y = makeInt("2")
val z = makeInt("3")

x.getOrElse(0) + y.getOrElse(0)

x map { a =>
  y map { b =>
    a + b
  }
}

x flatMap { a =>
  y flatMap { b =>
    z map { c =>
      a + b + c
    }
  }
}

val result234234 = for {
  x <- makeInt("1")
  y <- makeInt("2")
} yield x + y

makeInt("2").map(_ * 2)

val xs = Seq(1,2,3)
val ys = Seq(100,200,300)

for {
  x <- xs
  y <- ys
} yield x + y

implicit val executionContext: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

val f1 = scala.concurrent.Future { Thread.sleep(10*1000); 1 }
val f2 = scala.concurrent.Future { Thread.sleep(2*1000); 2 }
val f3 = scala.concurrent.Future { Thread.sleep(4*1000); 3 }

val result123 = for {
  r1 <- f1
  r2 <- f2
  r3 <- f3
} yield (r1 + r2 + r3)

result123 onComplete {
  case scala.util.Success(x) => println(x)
  case scala.util.Failure(e) => e.printStackTrace()
}

trait Functor[A] {
  def map[B](f: A => B): Functor[B]
}

def f(a: Int) = a * 2
def g(a: Int) =a * 3
g(f(100))

class Step2Debug {
  def bind(fun: Int => (Int,String), tup: (Int, String)): (Int, String) = {
    val (i,s) = tup;
    val (newI, newS) = fun(i)
    (newI, s + newS)
  }

  def f(a: Int): (Int, String) = {
    val result = a * 2
    (result, s"\nf result: $result.")
  }

  def g(a: Int): (Int, String) = {
    val result = a * 3
    (result, s"\ng result: $result.")
  }

  def h(a: Int): (Int, String) = {
    val result = a * 4
    (result, s"\nh result: $result.")
  }

  //  val (fInt, fString) = f(100)
  //  val (gInt, gString) = g(fInt)
  //  val debug = fString + " " + gString
  //
  //  println(s"result: $gInt, debug: $debug")

  val fResult = f(100)
  val gResult = bind(g, fResult)
  val hResult = bind(h, gResult)

  println(s"result: ${hResult._1}, debug: ${hResult._2}")
}

new Step2Debug()

//case class Wrapper[A](value: A) {
//  def map[B](f: A => B): Wrapper[B] = new Wrapper(f(value))
//  def flatMap[B](f: A => Wrapper[B]): Wrapper[B] = f(value)
//  override def toString: String = value.toString
//}

class Wrapper[A] private (value: A) {
  def map[B](f: A => B): Wrapper[B] = new Wrapper(f(value))
  def flatMap[B](f: A => Wrapper[B]): Wrapper[B] = f(value)
  override def toString: String = value.toString
}

object Wrapper {
  def apply[A](value: A): Wrapper[A] = new Wrapper(value)
}

val wx = Wrapper(1)
wx.map(_ * 2)

for { i <- wx } yield i * 2

for {
  a <- Wrapper(1)
  b <- Wrapper(2)
  c <- Wrapper(3)
} yield a + b + c

for {
  a <- Wrapper("a")
  b <- Wrapper("b")
  c <- Wrapper("c")
} yield a + b + c

class Step3Debug {
  case class Debuggable[A] (value: A, log: List[String]) {
    def map[B](f: A => B): Debuggable[B] = Debuggable(f(value), log)
    def flatMap[B](f: A => Debuggable[B]) = {
      val Debuggable(newValue, newLog) = f(value)
      Debuggable(newValue, log ::: newLog)
    }
  }

  def f(a: Int): Debuggable[Int] = {
    val result = a * 2
    Debuggable(result, s"f: a ($a) * 2 = $result." :: Nil)
  }
  def g(a: Int): Debuggable[Int] = {
    val result = a * 3
    Debuggable(result, s"f: a ($a) * 3 = $result." :: Nil)
  }
  def h(a: Int): Debuggable[Int] = {
    val result = a * 4
    Debuggable(result, s"f: a ($a) * 5 = $result." :: Nil)
  }

  println {
    for {
      fResult <- f(100)
      gResult <- g(fResult)
      hResult <- h(gResult)
    } yield hResult
  }

  println {
    f(100).flatMap { fResult =>
      g(fResult).flatMap { gResult =>
        h(gResult).map { hResult =>
          hResult
        }
      }
    }
  }
}

new Step3Debug

class IO[A] private (constructorCodeBlock: => A) {
  def run = constructorCodeBlock

  def flatMap[B](customAlgorithm: A => IO[B]): IO[B] = {
    val result1: IO[B] = customAlgorithm(run)
    val result2: B = result1.run
    IO(result2)
  }

  def map[B](f: A => B): IO[B] = flatMap(a => IO(f(a)))
}

object IO {
  def apply[A](constructorCodeBlock: => A): IO[A] = new IO(constructorCodeBlock)
}

def getLine: IO[String] = IO(scala.io.StdIn.readLine())
def putStrLn(s: String): IO[Unit] = IO(println(s))

for {
  _ <- putStrLn("First name?")
  //  firstName <- getLine
  _ <- putStrLn("Last name?")
  //  lastName <- getLine
} yield ()

def forExpression: IO[Unit] = for {
  _ <- putStrLn("First name?")
  _ <- putStrLn("Last name?")
} yield ()

forExpression.run

//val hello = cats.effect.IO { println("Hello, world") }
//hello.unsafeRunAsyncAndForget()

def readTextFileAsTry(filename: String): scala.util.Try[List[String]] = {
  scala.util.Try {
    val lines = using(scala.io.Source.fromFile(filename)) { source =>
      (for (line <- source.getLines()) yield line).toList
    }
    lines
  }
}

val passwdFile = readTextFileAsTry("/etc/password-foo")
passwdFile match {
  case scala.util.Success(lines) => lines.foreach(println)
  case scala.util.Failure(s) => println(s"Failed, message is: $s")
}

def readTextFileAsIO(filename: String): IO[List[String]] = {
  IO {
    val lines = using(scala.io.Source.fromFile(filename)) { source =>
      (for (line <- source.getLines()) yield line).toList
    }
    lines
  }
}

//readTextFileAsIO("/etc/password-foo").run

object Golfing {
  case class GolfState(strokes: List[Int])

  def nextStroke(
                  gs: GolfState,
                  distanceOfNextHit: Int): GolfState = {
    GolfState(distanceOfNextHit :: gs.strokes)
  }

  def push[A](xs: List[A], a: A): List[A] = a :: xs
  def pop[A](xs: List[A]): (A, List[A]) = (xs.head, xs.tail)

  def simulate() = {
    val state1 = GolfState(Nil)
    val state2 = nextStroke(state1, 15)
    val state3 = nextStroke(state2, 0)

    println(state3)
  }
}

Golfing.simulate()

//case class State(value: Int) {
//  def flatMap(f: Int => State): State = State(f(value).value)
//  def map(f: Int => Int): State = State(f(value))
//}
//
//for {
//  a <- State(20)
//  b <- State(a + 15)
//  c <- State(b + 0)
//} yield c

case class State[S,A](run: S => (S,A)) {
  //  def flatMap[B](f: A => State[S,B]): State[S,B] = State { s1: S =>
  //    val (s2, a) = run(s1)
  //    val stateChangeToB = f(a)
  //    val (s3, b) = stateChangeToB.run(s2)
  //    (s3, b)
  //  }

  def flatMap[B](f: A => State[S,B]): State[S,B] = State { s1: S =>
    val (s2, a) = run(s1)
    val (s3, b) = f(a).run(s2)
    (s3, b)
  }

  def map[B](f: A => B): State[S,B] = flatMap(a => State.lift(f(a)))
}

object State {
  def lift[S,A](value: A): State[S,A] = State(run = state => (state, value))
}

object Golfing3 {
  case class GolfState(distance: Int)

  def swing(distance: Int): State[GolfState, Int] = State { (s: GolfState) =>
    val newAmount = s.distance + distance
    (GolfState(newAmount), newAmount)
  }

  def simulate() = {
    val stateWithNewDistance: State[GolfState, Int] = for {
      _ <- swing(20)
      _ <- swing(15)
      totalDistance <- swing(0)
    } yield totalDistance

    val beginningState = GolfState(0)
    val result = stateWithNewDistance.run(beginningState)

    println(s"GolfState: ${result._1}")
    println(s"Total Distance: ${result._2}")
  }
}

Golfing3.simulate()

object MonadTransformers {
  trait Monad[M[_]] {
    def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B]
    def map[A,B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => lift(f(a)))
    def lift[A](a: A): M[A]
  }

  implicit val OptionMonad: Monad[Option] = new Monad[Option] {
    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma.flatMap(f)
    override def lift[A](a: A): Option[A] = Some(a)
  }

  implicit val IOMonad: Monad[IO] = new Monad[IO] {
    override def flatMap[A, B](ma: IO[A])(f: A => IO[B]): IO[B] = ma.flatMap(f)
    override def lift[A](a: A): IO[A] = IO(a)
  }

  case class StateT[M[_], S, A](run: S => M[(S,A)]) {
    def flatMap[B](g: A => StateT[M,S,B])(implicit M: Monad[M]): StateT[M,S,B] =
      StateT { (s0: S) =>
        M.flatMap(run(s0)) {
          case (s1,a) => g(a).run(s1)
        }
      }
    def map[B](f: A => B)(implicit M: Monad[M]): StateT[M,S,B] = flatMap(a => StateT.lift(f(a)))
  }

  object StateT {
    def lift[M[_],S,A](v: A)(implicit M: Monad[M]): StateT[M,S,A] = StateT(run = s => M.lift(s -> v))
  }

  case class IntState(i: Int)

  def add(i: Int) = StateT[IO, IntState, Int] { oldState: IntState =>
    val newValue = i + oldState.i
    val newState = oldState.copy(i = newValue)
    IO(newState, newValue)
  }

  def multiply(i: Int) = StateT[IO, IntState, Int] { oldState: IntState =>
    val newValue = i * oldState.i
    val newState = oldState.copy(i = newValue)
    IO(newState, newValue)
  }

  def test(): Unit = {
    val a = add(1)
    val b = a.run(IntState(1))
    b.map(t => println(s"b state = ${t._1}"))

    val forExpression: StateT[IO, IntState, Int] = for {
      _ <- add(2)
      _ <- add(3)
      x <- multiply(10)
    } yield x

    val result: IO[(IntState, Int)] = forExpression.run(IntState(1))
    result.map(tuple => println(s"IntState = ${tuple._1}"))
  }

  case class SumState(sum: Int)

  def updateAppState(newValue: Int): StateT[IO, SumState, Int] = StateT { (oldState:SumState) =>
    println(s"updateAppState($newValue)")
    val newSum = newValue + oldState.sum
    val newState = oldState.copy(sum = newSum)
    IO(newState, newSum)
  }

  def liftIoIntoStateT[A](io: IO[A]): StateT[IO, SumState, A] = StateT { s: SumState =>
    println(s"liftIoIntoStateT SumState.sum=${s.sum}")
    io.map(a => (s,a))
  }

  def sum: StateT[IO, SumState, Unit] = for {
    _ <- liftIoIntoStateT(putStrLn("give me an int"))
    i <- liftIoIntoStateT(IO(1))
    _ <- updateAppState(i)
  } yield ()

  def test2(): Unit = {
    sum.run(SumState(0)).run
  }
}

MonadTransformers.test()
MonadTransformers.test2()

type Money = BigDecimal

sealed trait Topping
case object Cheese extends Topping
case object Pepperoni extends Topping
case object Sausage extends Topping
case object Mushrooms extends Topping
case object Onions extends Topping

sealed trait CrustSize
case object SmallCrustSize extends CrustSize
case object MediumCrustSize extends CrustSize
case object LargeCrustSize extends CrustSize

sealed trait CrustType
case object RegularCrustType extends CrustType
case object ThinCrustType extends CrustType
case object ThickCrustType extends CrustType

sealed trait Product
trait BreadSticks extends Product
trait CheeseSticks extends Product

trait Beverage extends Product
trait BottledBeverage extends Beverage
trait CannedBeverage extends Beverage

//class Pizza {
//
//
//  var crustSize: CrustSize = MediumCrustSize
//  var crustType: CrustType = RegularCrustType
//
//  val toppings = scala.collection.mutable.ArrayBuffer[Topping]()
//
//  def addTopping(t: Topping): Unit = toppings += t
//  def removeTopping(t: Topping): Unit = toppings -= t
//  def removeAllToppings(): Unit = toppings.clear()
//
//  def getPrice(toppingPrices: Map[Topping, Money],
//               crustSizePrices: Map[CrustSize, Money],
//               crustTypePrices: Map[CrustType, Money]): Money = ???
//}
//
//class Customer(
//                var name: String,
//                var phone: String,
//                var address: Address
//              )
//
//class Address(
//               var street1: String,
//               var street2: Option[String],
//               var city: String,
//               var state: String,
//               var zipCode: String
//             )
//
//class Order {
//  val pizzas = scala.collection.mutable.ArrayBuffer[Pizza]()
//  var customer: Customer = null
//
//  def addPizzaToOrder(p: Pizza): Unit = pizzas += p
//  def removePizzaFromOrder(p: Pizza): Unit = pizzas -= p
//
//  def getBasePrice: Money = ???
//  def getTaxes: Money = ???
//  def getTotalPrice: Money = ???
//}

case class Pizza(crustSize: CrustSize,
                 crustType: CrustType,
                 toppings: Seq[Topping]) extends Product {
  def addTopping(t: Topping): Pizza = {
    val newToppings = this.toppings :+ t
    this.copy(toppings = newToppings)
  }
  def removeTopping(t: Topping): Pizza = this
  def removeAllTopping(): Pizza = {
    val newToppings = Seq[Topping]()
    this.copy(toppings = newToppings)
  }

  def updateCrustSize(cs: CrustSize): Pizza = this.copy(crustSize = cs)
  def updateCrustType(ct: CrustType): Pizza = this.copy(crustType = ct)

  def calculatePrice(toppingPrices: Map[Topping, Money],
                     crustSizePrices: Map[CrustSize, Money],
                     crustTypePrices: Map[CrustType, Money]): Money = {
    val base = BigDecimal(10)
    val numToppings = this.toppings.size
    val price = base + 1.00 * numToppings
    price
  }

  //  override def toString: String =
  //    s"""Pizza ($crustSize, $crustType), toppings = $toppings"""
}

case class Order(pizzas: Seq[Pizza],
                 customer: Customer)

case class Customer(name: String,
                    phone: String,
                    address: Address)

case class Address(street1: String,
                   street2: Option[String],
                   city: String,
                   state: String,
                   zipCode: String)

object Pizza {
  def addTopping(p: Pizza, t: Topping): Pizza = ???
  def removeTopping(p: Pizza, t: Topping): Pizza = ???
  def removeAllTopping(p: Pizza): Pizza = ???

  def updateCrustSize(p: Pizza, cs: CrustSize): Pizza = ???
  def updateCrustType(p: Pizza, ct: CrustSize): Pizza = ???

  def calculatePrice(p: Pizza,
                     toppingPrices: Map[Topping, Money],
                     crustSizePrices: Map[CrustSize, Money],
                     crustTypePrices: Map[CrustType, Money]): Money = ???
}

//val pizza2 = Pizza.addTopping(pizza1, Pepperoni)
//val pizza3 = Pizza.updateCrustSize(pizza2, LargeCrustSize)

trait PizzaServiceInterface {
  def addTopping(p: Pizza, t: Topping): Pizza
  def removeTopping(p: Pizza, t: Topping): Pizza
  def removeAllTopping(p: Pizza): Pizza

  def updateCrustSize(p: Pizza, cs: CrustSize): Pizza
  def updateCrustType(p: Pizza, ct: CrustType): Pizza

  def calculatePrice(p: Pizza,
                     toppingPrices: Map[Topping, Money],
                     crustSizePrices: Map[CrustSize, Money],
                     crustTypePrices: Map[CrustType, Money]): Money
}

trait PizzaService extends PizzaServiceInterface {
  def addTopping(p: Pizza, t: Topping): Pizza = {
    val newToppings = p.toppings :+ t
    p.copy(toppings = newToppings)
  }
  def removeTopping(p: Pizza, t: Topping): Pizza = p
  def removeAllTopping(p: Pizza): Pizza = {
    val newToppings = Seq[Topping]()
    p.copy(toppings = newToppings)
  }

  def updateCrustSize(p: Pizza, cs: CrustSize): Pizza = p.copy(crustSize = cs)
  def updateCrustType(p: Pizza, ct: CrustType): Pizza = p.copy(crustType = ct)

  def calculatePrice(p: Pizza,
                     toppingPrices: Map[Topping, Money],
                     crustSizePrices: Map[CrustSize, Money],
                     crustTypePrices: Map[CrustType, Money]): Money = {
    val base = BigDecimal(10)
    val numToppings = p.toppings.size
    val price = base + 1.00 * numToppings
    price
  }
}

trait PizzaDaoInterface {
  def getToppingPrices(): Map[Topping, Money]
  def getCrustSizePrices(): Map[CrustSize, Money]
  def getCrustTypePrices(): Map[CrustType, Money]
}

object MockPizzaDao extends PizzaDaoInterface {
  def getToppingPrices(): Map[Topping, Money] = Map(
    Cheese -> BigDecimal(1),
    Pepperoni -> BigDecimal(1),
    Sausage -> BigDecimal(1),
    Mushrooms -> BigDecimal(1)
  )
  def getCrustSizePrices(): Map[CrustSize, Money] = Map(
    SmallCrustSize -> BigDecimal(0),
    MediumCrustSize -> BigDecimal(1),
    LargeCrustSize -> BigDecimal(2)
  )
  def getCrustTypePrices(): Map[CrustType, Money] = Map(
    RegularCrustType -> BigDecimal(0),
    ThickCrustType -> BigDecimal(1),
    ThinCrustType -> BigDecimal(1)
  )
}

trait OrderServiceInterface {
  protected def database:PizzaDaoInterface    // MockPizzaDao, TestPizzaDao, or ProductionPizzaDao
  def calculateOrderPrice(o: Order): Money
  def addPizzaToOrder(o: Order, p: Pizza): Order
}

trait AbstractOrderService extends OrderServiceInterface {
  object PizzaService extends PizzaService
  import PizzaService.calculatePrice

  private lazy val toppingPricesMap = database.getToppingPrices()
  private lazy val crustSizePricesMap = database.getCrustSizePrices()
  private lazy val crustTypePricesMap = database.getCrustTypePrices()

  override def calculateOrderPrice(o: Order): Money =
    calculateOrderPriceInternal(
      o,
      toppingPricesMap,
      crustSizePricesMap,
      crustTypePricesMap
    )

  override def addPizzaToOrder(o: Order, p: Pizza): Order = o.copy(pizzas = o.pizzas :+ p)

  private def calculateOrderPriceInternal(o: Order,
                                          toppingPrices: Map[Topping, Money],
                                          crustSizePrices: Map[CrustSize, Money],
                                          crustTypePrices: Map[CrustType, Money]): Money = {
    val pizzaPrices: Seq[Money] = for {
      pizza <- o.pizzas
    } yield {
      calculatePrice(
        pizza,
        toppingPrices,
        crustSizePrices,
        crustTypePrices
      )
    }
    pizzaPrices.sum
  }
}

object MockDbOrderService extends AbstractOrderService {
  val database = MockPizzaDao
}

object MainDriver {
  def execute(): Unit = {
    object PizzaService extends PizzaService
    import PizzaService._
    val address = Address("1 Main St", None, "Talkeetna", "AK", "99676")
    val customer = Customer("Alvin Alexander", "093-222-4232", address)
    val o1 = Order(Seq[Pizza](), customer)
    val p1 = Pizza(MediumCrustSize, RegularCrustType, Seq(Cheese))
    val o2 = MockDbOrderService.addPizzaToOrder(o1, p1)
    val p2 = Pizza(MediumCrustSize, RegularCrustType, Seq(Cheese))
    val p2a = addTopping(p2, Pepperoni)
    val o3 = MockDbOrderService.addPizzaToOrder(o2, p2a)
    println(o3)

    val p2d = updateCrustSize(
      updateCrustType(
        addTopping(
          addTopping(p2, Pepperoni),
          Mushrooms
        ),
        ThickCrustType
      ),
      LargeCrustSize
    )
    val orderPrice = MockDbOrderService.calculateOrderPrice(o3)
    println(s"Order Price: $orderPrice")
    val p5 = Pizza(
      MediumCrustSize,
      RegularCrustType,
      Seq(Cheese, Pepperoni, Pepperoni, Sausage)
    )
    val p5a = removeAllTopping(p5)
    println(p5a)
  }
}

MainDriver.execute()

object Driver {
  def execute(): Unit = {
    val toppingPrices = MockPizzaDao.getToppingPrices()
    val crustSizePrices = MockPizzaDao.getCrustSizePrices()
    val crustTypePrices = MockPizzaDao.getCrustTypePrices()

    val pizza1 = Pizza(
      MediumCrustSize,
      ThinCrustType,
      Seq(Cheese, Pepperoni)
    )

    val pizza2 = pizza1.addTopping(Mushrooms)
    val pizza3 = pizza2.updateCrustSize(LargeCrustSize)
    println(s"pizza3: $pizza3")

    val pizzaPrice = pizza3.calculatePrice(
      toppingPrices,
      crustSizePrices,
      crustTypePrices
    )
    println(s"price of pizza3: $pizzaPrice")

    val pizza4 = pizza1.addTopping(Mushrooms)
      .updateCrustSize(LargeCrustSize)
      .updateCrustType(ThickCrustType)
    println(s"pizza4: $pizza4")
  }
}

Driver.execute()

trait Animal
sealed trait Color
case object Red extends Color
abstract class AnimalWithTail(tailColor: Color) extends Animal

trait DogTailServices {
  this: AnimalWithTail =>
  def wagTail = println("wagging tail")
  def lowerTail = println("lowering tail")
  def raiseTail = println("raising tail")
}

trait DogMouthServices {
  this: AnimalWithTail =>
  def bark = println("bark!")
  def lick = println("licking")
}

object IrishSetter
  extends AnimalWithTail(Red)
    with DogTailServices
    with DogMouthServices

IrishSetter.bark
IrishSetter.wagTail

def readTextFileAsString(filename: String): scala.util.Try[String] = {
  scala.util.Try {
    val lines = using(scala.io.Source.fromFile(filename)) { source =>
      (for (line <- source.getLines()) yield line).toList
    }
    lines.mkString("\n")
  }
}

def readTextFileAsStringIO(filename: String): IO[scala.util.Try[String]] = {
  IO(readTextFileAsString(filename))
}

val pwdFile = readTextFileAsStringIO("/etc/passwdFoo")
//pwdFile match {
//  case scala.util.Success(s) => println(s)
//  case scala.util.Failure(e) => println(e)
//}

//for {
//  a <- pwdFile
//  b <- a
//} yield a

trait BehavesLikeHuman[A] {
  def speak(a: A): Unit
}

object BehavesLikeHumanInstances {
  implicit val dogBehavesLikeHuman: BehavesLikeHuman[Dog] = new BehavesLikeHuman[Dog] {
    override def speak(dog: Dog): Unit = println(s"I'm a Dog, my name is ${dog.name}")
  }
}

object BehavesListHumanSyntax {
  implicit class BehavesLikeHumanOps[A](value: A) {
    def speak(implicit behavesLikeHumanInstance: BehavesLikeHuman[A]): Unit = {
      behavesLikeHumanInstance.speak(value)
    }
  }
}

object BehavesLikeHuman {
  def speak[A](a: A)(implicit behaveLikeHumanInstance: BehavesLikeHuman[A]): Unit = {
    behaveLikeHumanInstance.speak(a)
  }
}

final case class Dog(name: String) extends Animal
final case class Cat(name: String) extends Animal
final case class Bird(name: String) extends Animal

import BehavesListHumanSyntax.BehavesLikeHumanOps
val rover = Dog("Rover")
BehavesLikeHuman.speak(rover)(BehavesLikeHumanInstances.dogBehavesLikeHuman)
rover.speak(BehavesListHumanSyntax.BehavesLikeHumanOps[Dog])

trait ToString[A] {
  def toString(a: A): String
}

implicit val pizzaAsString: ToString[Pizza] = (p: Pizza) => {
  s"""Pizza(${p.crustSize},${p.crustType}), toppings = ${p.toppings}"""
}

object ToStringSyntax {
  implicit class ToStringOps[A](value: A) {
    def asString(implicit toStringInstance: ToString[A]): String = {
      toStringInstance.toString(value)
    }
  }
}

//implicit class ToStringOps[A](value: A) {
//  def asString(implicit toStringInstance: ToString[A]): String = {
//    toStringInstance.toString(value)
//  }
//}

import ToStringSyntax._

val p = Pizza(
  LargeCrustSize,
  ThinCrustType,
  Seq(Cheese, Pepperoni, Sausage)
)
p.asString

import cats.Show
import cats.syntax.show._

implicit val pizzaShow: Show[Pizza] = Show.show[Pizza] { p =>
  s"""Pizza(${p.crustSize}, ${p.crustType}),
     |    toppings = ${p.toppings}
     |""".stripMargin
}

p.show

object SimpleBadConcurrency {
  class Person(var name: String, var town: String, var state: String) {
    override def toString: String = s"name: $name, town: $town, state: $state"
  }

  def simulate(): Unit = {
    val me = new Person("Alvin","Talkeetna", "Alaska")
    val t1 = new Thread {
      override def run(): Unit = {
        Thread.sleep(1000)
        me.town = "Boulder"
        Thread.sleep(3000)
        me.state = "Colorado"
      }
    }
    t1.start()
    println(s"1) $me")
    Thread.sleep(2000)
    println(s"2) $me")
    Thread.sleep(2000)
    println(s"3) $me")
  }
}

SimpleBadConcurrency.simulate()

case class Hello(msg: String)
//class HelloActor extends akka.actor.Actor {
//  override def receive: Receive = {
//    case Hello(s) =>
//      println(s"you said '$s'")
//      println(s"$s back at you!")
//    case _ => println("huh?")
//  }
//}

//object AkkaHelloWorld {
//  val system = akka.actor.ActorSystem("HelloSystem")
//
//  def simulate(): Unit = {
//    val helloActor = system.actorOf(akka.actor.Props[HelloActor], name = "helloActor")
//    helloActor ! Hello("hello")
//    helloActor ! Hello("buenos dias")
//    helloActor ! "hi!"
//    system.terminate()
//  }
//}

//AkkaHelloWorld.simulate()

// scala.concurrent.ConcurrentTask
// scala.concurrent.AsynchronousTask
// scala.concurrent.NonBlockingParallelTask
val aa = scala.concurrent.Future { Thread.sleep(1000); 42 }
val bb = aa.map(_ * 2)

aa.onComplete {
  case scala.util.Success(value) => println(s"Got the callback, value = $value")
  case scala.util.Failure(e) => e.printStackTrace()
}

aa.andThen {
  case scala.util.Success(value) => println(s"Got the callback, value = $value")
  case scala.util.Failure(e) => e.printStackTrace()
}

aa.isCompleted
aa.value
aa.foreach(v => println(v))

object MultipleFutures {
  def test(): Unit = {
    val f1 = scala.concurrent.Future { Thread.sleep(800); 1 }
    val f2 = scala.concurrent.Future { Thread.sleep(200); 2 }
    val f3 = scala.concurrent.Future { Thread.sleep(400); 3 }
    val result = for {
      r1 <- f1
      r2 <- f2
      r3 <- f3
    } yield r1 + r2 + r3
    result.onComplete {
      case scala.util.Success(value) => println(s"result = $value")
      case scala.util.Failure(e) => e.printStackTrace()
    }
  }
}

MultipleFutures.test()

Thread.sleep(3000)

val ff1 = scala.concurrent.Future { Thread.sleep(500); 1 }
val rez = ff1.transform(
  i => i * 42,
  e => new Exception("something bad happened: " + e)
)
// transformWith
// failed
// mapTo
// recover
// recoverWith
rez.value
Thread.sleep(600)
rez.value

val add1 = (i: Int) => i + i
val add11: Int => Int = i => i + 1

val isEven = (i: Int) => i % 2 == 0
val isEven2: Int => Boolean = i => i % 2 == 0
val isEven3 = new Function1[Int,Boolean] {
  override def apply(i: Int): Boolean = i % 2 == 0
}
def isEven4(i: Int): Boolean = i % 2 == 0

val ff: Any => String = {
  case _: Int => "Int"
  case _: Double => "Double"
  case _ => "Other"
}
ff(1)

List(1,2,3,4).filter({ case i: Int => i % 2 == 0 })
(add1 andThen add1)(5)
(add1 compose add1)(10)
add1.toString

def firstChar[A](a: A) = a.toString.charAt(0)
def lengthOfThing[A] = (a: A) => a.toString.length
val fff = lengthOfThing[Int]
fff(694)

def sum(list: List[Int]): Int = list.reduce(_ + _)

List(1,2,3,4).reduceLeft((x: Int, y: Int) => {
  val theDiff = x - y
  println(s"received $x and $y, their difference is $theDiff")
  theDiff
})
List(2,3,4).foldLeft(1)((x: Int, y: Int) => {
  val theDiff = x - y
  println(s"received $x and $y, their difference is $theDiff")
  theDiff
})
List(2,3,4).scanLeft(1)((x: Int, y: Int) => {
  val theDiff = x - y
  println(s"received $x and $y, their difference is $theDiff")
  theDiff
})
List(1,2,3,4).reduceRight((x: Int, y: Int) => {
  val theDiff = x - y
  println(s"received $x and $y, their difference is $theDiff")
  theDiff
})
List(1,2,3).foldRight(4)((x: Int, y: Int) => {
  val theDiff = x - y
  println(s"received $x and $y, their difference is $theDiff")
  theDiff
})
List(1,2,3).scanRight(4)((x: Int, y: Int) => {
  val theDiff = x - y
  println(s"received $x and $y, their difference is $theDiff")
  theDiff
})
List("foo","bar","baz").reduceLeft(_ + _)
List("foo","bar","baz").reduceRight(_ + _)

object FoldLeftInt {
  val a = List(1,2,3,4)
  def add(a: Int, b: Int) = a + b
  def foldLeft[A](lastResult: A)(list: List[A])(f: (A, A) => A): A = list match {
    case Nil => lastResult
    case x :: xs => {
      val result = f(lastResult, x)
      println(s"last: $lastResult, x: $x, result = $result")
      foldLeft(result)(xs)(f)
    }
  }
  def test() = {
    println(foldLeft(0)(a)(_ + _))
  }
}

FoldLeftInt.test()

val xs2 = List(1,2,3)
val ys2 = List(4,5,6)
val zs = List(7,8,9)

for { x <- xs2 } println(x)
xs2.foreach(x => println(x))

for { x <- xs2 } yield x
xs.map(x => x)

for { x <- xs2; y <- ys2; z <- xs } yield x + y + z
xs2.flatMap { x => ys2.flatMap { y => zs.map(z => x + y + z) } }

for { x <- xs2 if x < 2 } yield x
xs2.withFilter(x => x < 2).map(x => x)

case class Person2(name: String, relation: String)
val emily = Person2("Emily", "niece")
emily.name

case class Company(var name: String)
val c2 = Company("Mat-Su Valley Programming")
c2.name
c2.name = "Valley Programming"

emily.toString
emily match {
  case Person2(n,r) => println(n,r)
}
val emily2 = Person2("Emily", "niece")
emily == emily2

case class Employee(name: String, loc: String, role: String)
val fred = Employee("Fred","Anchorage","Salesman")
val joe2 =  fred.copy(name = "Joe", role = "Mechanic")

case class Pair(
                 a: Int,
                 b: Int
               )

sealed trait Direction
case object North extends Direction
case object South extends Direction
case object East extends Direction
case object West extends Direction

sealed trait Bool
case object True extends Bool
case object False extends Bool

case class DoubleBoo (b1: Bool, b2: Bool)
DoubleBoo(True,True)
DoubleBoo(True,False)
DoubleBoo(False,True)
DoubleBoo(False,False)

sealed trait Shape
final case class Circle(radius: Double) extends Shape
final case class Rectangle(width: Double, height: Double) extends Shape

def isRound(s: Shape): Boolean = s match {
  case Circle(_) => true
  case _ => false
}

def or(a: Bool, b: Bool): Bool = (a,b) match {
  case (True,_) => True
  case (_,True) =>True
  case (_,_) => False
}

val x2 = List.range(1,10)
x2.filter((i:Int)=>i%2==0)
x2.filter(i=>i%2==0)
x2.filter(_%2==0)

val map = Map(
  1 -> 10,
  2 -> 20,
  3 -> 30
)

val newMap = map.transform((k,v) => k + v)

trait SentientBeing {
  def id: Int
}

class Person3 extends SentientBeing {
  def id = 2
}