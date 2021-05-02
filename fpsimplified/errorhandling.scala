def makeInt(s: String): Option[Int] = {
  try {
    Some(s.toInt)
  } catch {
    case _: Exception => None
  }
}

makeInt("foo") match {
  case Some(i) => s"i = $i"
  case None => "toInt could not parse 'input'"
}

val result = makeInt("bar").getOrElse(0)

for {
  x <- makeInt("1")
  y <- makeInt("2")
  z <- makeInt("3")
} yield x + y + z

import scala.util.{Try, Success, Failure}

def tryMakeInt(s: String) = Try(s.trim.toInt)

tryMakeInt("1")
tryMakeInt("foo")

tryMakeInt("hello") match {
  case Success(value) => s"Success, value is: $value"
  case Failure(exception) => s"Failure, exception is $exception"
}

for {
  a <- tryMakeInt("1")
  b <- tryMakeInt("10")
} yield a + b

def eitherMakeInt(s: String): Either[String,Int] = {
  try {
    Right(s.trim.toInt)
  } catch {
    case e: Exception => Left(e.toString)
  }
}

eitherMakeInt("1")
eitherMakeInt("foo")

eitherMakeInt("11") match {
  case Left(s) => s"Error message: $s"
  case Right(i) => s"Desired answer: $i"
}

for {
  a <- eitherMakeInt("1")
  b <- eitherMakeInt("10")
} yield a + b