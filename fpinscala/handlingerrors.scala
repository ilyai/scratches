import scala.annotation.tailrec

def failingFn(i: Int): Int = {
  val x: Int = throw new Exception("fail!")
  try {
    val y = 42 + 5
    x + y
  } catch {
    case _: Exception => 43
  }
}

//failingFn(1)

def mean(xs: Seq[Double]): Double =
  if (xs.isEmpty)
    throw new ArithmeticException("mean of empty list!")
  else xs.sum / xs.length

//mean(Nil)
mean(Seq(1,2))

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(v) => Some(f(v))
  }
  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(v) => v
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case Some(_) => this
  }
  def orElse_2[B >: A](ob: => Option[B]): Option[B] = this map (Some(_)) getOrElse ob
  def filter(f: A => Boolean): Option[A] = flatMap(v => if (f(v)) Some(v) else None)
  def lift[A,B](f: A => B): Option[A] => Option[B] = _.map(f)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
    a.flatMap(aa =>
      b.map(bb =>
        f(aa,bb)
      )
    )
  def map2_2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa,bb)
}

def mean2(xs: Seq[Double]): Option[Double] =
  if (xs.isEmpty) None
  else Some(xs.sum / xs.length)

mean2(Seq(1,2))

case class Employee(name: String, dept: String)
val employeesByName: Map[String, Employee] = List(
  Employee("Bob", "R&D"),
  Employee("Alice", "Accounting")
).map(e => e.name -> e).toMap

val dept: String = employeesByName
  .get("Joe")
  .map(_.dept)
  .filter(_ != "Accounting")
  .getOrElse("Default Dept")

def variance(xs: Seq[Double]): Option[Double] =
  mean2(xs).flatMap(m => mean2(xs.map(x => math.pow(x-m, 2))))

variance(Nil)
variance(Seq(1,2,3))

def pattern(s: String): Option[java.util.regex.Pattern] =
  try {
    Some(java.util.regex.Pattern.compile(s))
  } catch {
    case _: java.util.regex.PatternSyntaxException => None
  }

def mkMatcher(pat: String): Option[String => Boolean] =
  pattern(pat) map (p => (s:String) => p.matcher(s).matches())

def mkMatcher2(pat: String): Option[String => Boolean] =
  Some(1).lift((p:java.util.regex.Pattern) => (s:String) => p.matcher(s).matches())(pattern(pat))

def mkMatcher3(pat: String): Option[String => Boolean] =
  for {
    p <- pattern(pat)
  } yield (s:String) => p.matcher(s).matches()

def doesMatch(pat: String, s: String): Option[Boolean] =
  for {
    p <- mkMatcher3(pat)
  } yield p(s)

def bothMatch(pat1: String, pat2: String, s: String): Option[Boolean] =
  for {
    p1 <- mkMatcher3(pat1)
    p2 <- mkMatcher3(pat2)
  } yield p1(s) && p2(s)

def bothMatch_2(pat1: String, pat2: String, s: String): Option[Boolean] =
  mkMatcher3(pat1).flatMap(p1 =>
    mkMatcher3(pat2).map(p2 =>
      p1(s) && p2(s)
    )
  )

def bothMatch_3(pat1: String, pat2: String, s: String): Option[Boolean] =
  Option.map2(mkMatcher(pat1),mkMatcher(pat2))((p1,p2) => p1(s) && p2(s))

def sequence[A](a: List[Option[A]]): Option[List[A]] = {
  @tailrec
  def loop(l: List[Option[A]], acc: List[A]): List[A] = l match {
    case Nil => acc
    case h :: t => h match {
      case None => Nil
      case Some(v) => loop(t, acc :+ v)
    }
  }
  Some(loop(a,Nil)).filter(_.nonEmpty)
}

def sequence_3[A](a: List[Option[A]]): Option[List[A]] = a match {
  case Nil => Some(Nil)
  case h :: t => h.flatMap(hh => sequence_3(t).map(hh :: _))
}

def sequence_4[A](a: List[Option[A]]): Option[List[A]] =
  a.foldRight(Some(Nil):Option[List[A]])((x,y) => Option.map2(x,y)(_ :: _))

def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
  @tailrec
  def loop(l: List[A], acc: List[B]): List[B] = l match {
    case Nil => acc
    case h :: t => f(h) match {
      case None => Nil
      case Some(v) => loop(t, acc :+ v)
    }
  }
  Some(loop(a,Nil)).filter(_.nonEmpty)
}

def sequence_2[A](a: List[Option[A]]): Option[List[A]] =
  traverse(a)(a => a)

def traverse_2[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
  case Nil => Some(Nil)
  case h::t => Option.map2(f(h), traverse_2(t)(f))(_ :: _)
}

def traverse_3[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] =
  a.foldRight(Some(Nil):Option[List[B]])((h,t) => Option.map2(f(h),t)(_ :: _))


sequence(List(Some(1), Some(2), None, Some(4)))   // => None
sequence_2(List(Some(1), Some(2), None, Some(4)))   // => None
sequence(List(Some(1), Some(2), Some(3)))   // => Some(List(1,2,3))
sequence_2(List(Some(1), Some(2), Some(3)))   // => Some(List(1,2,3))

sealed trait Either[+E,+A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }
  def flatMap[EE >: E, B](f: A => Either[EE,B]): Either[EE,B] =
    map(f) match {
      case Right(a) => a
      case Left(e) => Left(e)
    }
  def flatMap_2[EE >: E, B](f: A => Either[EE,B]): Either[EE,B] = this match {
    case Right(a) => f(a)
    case Left(e) => Left(e)
  }
  def orElse[EE >: E,B >: A](b: => Either[EE,B]): Either[EE,B] = this match {
    case Left(_) => b
    case Right(a) => Right(a)
  }
  def map2[EE >: E, B, C](b: Either[EE,B])(f: (A,B) => C): Either[EE,C] =
    flatMap(a =>
      b.map(bb =>
        f(a,bb)
      )
    )
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

//case class LeftM[+E](value: List[E]) extends Either[E, Nothing]

object Either {
  def Try[A](a: => A): Either[Exception,A] = try Right(a) catch {
    case e: Exception => Left(e)
  }
  // a: List(1,2,3)
  // f: 1 => Right("1"), 2 => Right("2"), 3 => Left("Too big")
  // f: 1 => Right("1"), 2 => Right("2"), 3 => Right("3")
  // result: Left("Too big") | Right(List("1","2","3"))
  def traverse[E,A,B](a: List[A])(f: A => Either[E,B]): Either[E,List[B]] = {
    def go(a: List[A], acc: Either[E,List[B]]): Either[E,List[B]] = a match {
      case Nil => acc
      case h :: t => f(h) match {
        case Left(e) => Left(e)
        case Right(b) => go(t, acc.map(_ :+ b))
      }
    }
    go(a, Right(Nil))
  }
  def traverse_2[E,A,B](a: List[A])(f: A => Either[E,B]): Either[E,List[B]] = a match {
    case Nil => Right(Nil)
    case h::t => f(h).map2(traverse_2(t)(f))(_ :: _)
  }
  def traverse_3[E,A,B](a: List[A])(f: A => Either[E,B]): Either[E,List[B]] =
    a.foldRight(Right(Nil):Either[E,List[B]])((h,t) => f(h).map2(t)(_ :: _))
  def sequence[E,A,B](a: List[Either[E,A]]): Either[E,List[B]] =
    traverse(a)(a => a.asInstanceOf[Either[E,B]])
}

Either.sequence(List(Right(1),Right(2),Right(3)))
Either.sequence(List(Right(1),Right(2),Left("Too big")))

def mean3(xs: IndexedSeq[Double]): Either[String, Double] =
  if (xs.isEmpty)
    Left("mean of empty list!")
  else Right(xs.sum / xs.length)

for {
  age <- Right(42)
  name <- Left("invalid name")
  salary <- Right(1000000.0)
} yield Employee(name, "R&D")

sealed class Name(value: String)
sealed class Age(value: Int)
case class Person(name: Name, age: Age)

def mkName(name: String): Either[String, Name] =
  if (name == null || name == "") Left("Name is empty")
  else Right(new Name(name))

def mkAge(age: Int): Either[String, Age] =
  if (age < 0) Left("Age is out of range")
  else Right(new Age(age))

def mkPerson(name: String, age: Int): Either[String, Person] =
  mkName(name).map2(mkAge(age))(Person)

def mkPerson_2(name: String, age: Int): Either[List[String], Person] = {
  val nameOrError = mkName(name)
  val ageOrError = mkAge(age)
  (nameOrError,ageOrError) match {
    case (Right(n),Right(a)) => Right(Person(n,a))
    case _ => Left(List(nameOrError,ageOrError).foldLeft(Nil:List[String])((acc,a) => a match {
      case Left(e) => e :: acc
      case _ => acc
    }))
  }
}

mkPerson("John", 40)
mkPerson_2("John", 40)
mkPerson("", 40)
mkPerson_2("", 40)
mkPerson("John", -1)
mkPerson_2("John", -1)
mkPerson("", -1)
mkPerson_2("", -1)