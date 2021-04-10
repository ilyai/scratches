// Type Class

sealed trait Json
final case class JsObject(get: Map[String,Json]) extends Json
final case class JsString(get: String) extends Json
final case class JsNumber(get: Double) extends Json
case object JsNull extends Json

trait JsonWriter[-A] {
  def write(value: A): Json
}

final case class Person(name: String, email: String)

// Type Class Instances
object JsonWriterInstances {
  implicit val stringWriter: JsonWriter[String] =
    new JsonWriter[String] {
      override def write(value: String): Json = JsString(value)
    }

  implicit val personWriter: JsonWriter[Person] =
    new JsonWriter[Person] {
      override def write(value: Person): Json =
        JsObject(Map(
          "name" -> JsString(value.name),
          "email" -> JsString(value.email)
        ))
    }
}

// Type Class Interfaces
object Json {
  def toJson[A](value: A)(implicit w: JsonWriter[A]): Json = w.write(value)
}

import JsonWriterInstances._
Json.toJson(Person("Dave", "dave@example.com"))

object JsonSyntax {
  implicit class JsonWriterOps[A](value: A) {
    def toJson(implicit w: JsonWriter[A]): Json = w.write(value)
  }
}

import JsonSyntax._
Person("Dave", "dave@example.com").toJson

val jsonWriter = implicitly[JsonWriter[String]]

implicit def optionWriter[A](implicit writer: JsonWriter[A]): JsonWriter[Option[A]] =
  new JsonWriter[Option[A]] {
    override def write(value: Option[A]): Json = value match {
      case Some(v) => writer.write(v)
      case None => JsNull
    }
  }

Json.toJson(Option("A string"))
Json.toJson(Option("A string"))(optionWriter[String])
Json.toJson(Option("A string"))(optionWriter(stringWriter))

trait Printable[A] {
  def format(value: A): String
}

object PrintableInstances {
  implicit val stringPrintable: Printable[String] = (value: String) => value
  implicit val intPrintable: Printable[Int] = (value: Int) => value.toString
  implicit val catPrintable: Printable[Cat] = (value: Cat) => s"${value.name} is a ${value.age} year-old ${value.color} cat."
}

object Printable {
  def format[A](value: A)(implicit p: Printable[A]) = p.format(value)
  def print[A](value: A)(implicit p: Printable[A]) = println(value)
}

object PrintableSyntax {
  implicit class PrintableOps[A](value: A) {
    def format(implicit p: Printable[A]) = p.format(value)
    def print(implicit p: Printable[A]): Unit = println(value)
  }
}

final case class Cat(name: String, age: Int, color: String)

val cat = Cat("Tommy", 3, "black")
import PrintableInstances._
Printable.format(cat)
import PrintableSyntax._
cat.format
cat.print

import cats.Show
import cats.instances.int._
import cats.instances.string._

val showInt = Show.apply[Int]
val showString = Show.apply[String]
val intAsString = showInt.show(123)
val stringAsString = showString.show("abc")

import cats.syntax.show._
123.show
"abc".show

//import cats._
//import cats.implicits._
//import cats.instances.all._
//import cats.syntax.all._

import java.util.Date
//implicit val dateShow: Show[Date] =
//  new Show[Date] {
//    override def show(t: Date): String =
//      s"${t.getTime}ms since the epoch"
//  }

//implicit val dateShow: Show[Date] =
//  Show.show(date => s"${date.getTime}ms since the epoch")

implicit val dateShow: Show[Date] = (date: Date) => s"${date.getTime}ms since the epoch"
implicit val catShow: Show[Cat] = (value: Cat) => s"${value.name} is a ${value.age} year-old ${value.color} cat."

new Date().show
cat.show

import cats.Eq
import cats.instances.int._
val eqInt = Eq[Int]
//List(1,2,3).map(Option(_)).filter(_ == 1)
//List(1,2,3).map(Option(_)).filter(eqInt.eqv(_,1))
import cats.syntax.eq._
123 === 123
123 =!= 234

import cats.instances.option._
Option(1) === Option.empty[Int]

import cats.syntax.option._
1.some === none[Int]
1.some =!= none[Int]

import cats.instances.long._
implicit val dateEq: Eq[Date] =
  Eq.instance[Date] { (d1,d2) => d1.getTime === d2.getTime }

val x = new Date()
val y = new Date()

x === x
y === x

implicit val catEq: Eq[Cat] =
  Eq.instance[Cat] { (cat1,cat2) =>
    cat1.name === cat2.name &&
      cat1.age === cat2.age &&
      cat1.color === cat2.color
  }

val cat1 = Cat("Garfield", 38, "orange and black")
val cat2 = Cat("Healthcliff", 33, "orange and black")
cat1 === cat2
Option(cat1) === Option.empty[Cat]

sealed trait Shape
case class Circle(radius: Double) extends Shape

val circles: List[Circle] = List(Circle(5.4))
val shapes: List[Shape] = List(Circle(2.1))

val shapeWriter: JsonWriter[Shape] =
  new JsonWriter[Shape] {
    override def write(value: Shape): Json = JsNull
  }

val circleWriter: JsonWriter[Circle] =
  new JsonWriter[Circle] {
    override def write(value: Circle): Json = JsObject(Map("circle" -> JsNumber(value.radius)))
  }

def format[A](value: A, writer: JsonWriter[A]): Json =
  writer.write(value)

format(Circle(3.1), shapeWriter)
format(Circle(3.1), circleWriter)
format(new Shape {}, shapeWriter)
//format(new Shape {}, circleWriter)