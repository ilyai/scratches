import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure,Success}

var a = Future { Thread.sleep(1000); 42 }
val b = a.map(_ * 2)
b

a onComplete {
  case Success(value) => println(s"got the callback, value = $value")
  case Failure(e) => e.printStackTrace()
}

val result: Future[Int] = for {
  r1 <- Future { Thread.sleep(800); 1 }
  r2 <- Future { Thread.sleep(800); 1 }
  r3 <- Future { Thread.sleep(800); 1 }
} yield (r1 + r2 + r3)

Thread.sleep(4000)

result onComplete {
  case Success(x) => println(s"result = $x")
  case Failure(e) => e.printStackTrace()
}

