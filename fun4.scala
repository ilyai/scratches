def failingFn(i: Int): Int = {
  val y: Int = throw new Exception("fail")
  try {
    val x = 42 + 5
    x + y
  } catch {
    case e: Exception => 43
  }
}

//failingFn(12)

def failingFn2(i: Int): Int = {
  try {
    val x = 42 + 5
    x + ((throw new Exception("fail")): Int)
  } catch {
    case e: Exception => 43
  }
}

failingFn2(14)


def mean(xs: Seq[Double]): Double =
  if(xs.isEmpty) throw new ArithmeticException("mean of empty list!")
  else xs.sum / xs.length

def mean1(xs: IndexedSeq[Double], onEmpty: Double): Double =
  if(xs.isEmpty) onEmpty
  else xs.sum / xs.length

def mean2(xs: Seq[Double]): Option[Double] =
  if(xs.isEmpty) None
  else Some(xs.sum / xs.length)

def Try[A](a: => A): Option[A] = try Some(a) catch { case e: Exception => None }

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case _ => None
  }
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case _ => None
  }
  def flatMap2[B](f: A => Option[B]): Option[B] = this.map(f).getOrElse(None)
  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case _ => default
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case Some(a) => Some(a)
    case _ => ob
  }
  def orElse2[B >: A](ob: => Option[B]): Option[B] = this.map(Some(_)).getOrElse(ob)
  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => Some(a)
    case _ => None
  }
  def filter2(f: A => Boolean): Option[A] = this.flatMap(a => if (f(a)) Some(a) else None)
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = {
    a.flatMap(av => b.map(bv => Some(f(av,bv))))
  }
  def map22[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = {
    for {
      av <- a
      bv <- b
    } yield f(av,bv)
  }
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h.flatMap(hv => sequence(t).map(hv :: _))
  }
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    traverse(a)(x => x)
  }
  def parseInts(a: List[String]): Option[List[Int]] = sequence(a.map(i => Try(i.toInt)))
  def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
  }
}

Some(1).map(_ + 1)
None.map((i: Int) => i * 2)
Some(1).flatMap(a => Some(a + 1))
None.orElse(Some(4))
Some(5).filter(_ > 4)

def variance(xs: Seq[Double]): Option[Double] =
  mean2(xs).flatMap(m => mean2(xs.map(x => math.pow(x - m, 2))))

def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

variance(List(1.1,2.5,1,9))

val abs0: Option[Double] => Option[Double] = lift(math.abs)


def parseInsuranceRateQuote(age: String, numberOfTickets: String): Option[Double] = {
  val optAge: Option[Int] = Try(age.toInt)
  val optTickets: Option[Int] = Try(numberOfTickets.toInt)
}

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(a) => Right(a)
  }
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A,B) => C): Either[EE, C] = for {
    a <- this
    bb <- b
  } yield f(a,bb)

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(x => x)

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil => Right(Nil)
    case h :: t => f(h).map2(traverse(t)(f))(_ :: _)
  }
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

def saveDivision(x: Int, y: Int): Either[Exception, Int] = {
  try Right(x / y)
  catch {
    case e: Exception =>
    Left(e)
  }
}

def TryEither[A](op: => A): Either[Exception, A] = {
  try Right(op)
  catch {
    case e: Exception =>
      Left(e)
  }
}

case class Person(name: Name, age: Age)
sealed class Name(val value: String)
sealed class Age(val value: Int)

def mkName(name: String): Either[String, Name] =
  if (name == "" || name == null) Left("Name is empty")
  else Right(new Name(name))

def mkAge(age: Int): Either[String, Age] =
  if (age < 0) Left("Age is out of range")
  else Right(new Age(age))

def mkPerson(name: String, age: Int): Either[String, Person] =
  mkName(name).map2(mkAge(age))(Person(_, _))

trait Partial[+A,+B]
case class Errors[+A](get: Seq[A]) extends Partial[A, Nothing]
case class Success[+B](get: B) extends Partial[Nothing, B]