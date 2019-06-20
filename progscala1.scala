class Upper {
  def upper(strings: String*): Seq[String] = {
    strings.map((s:String) => s.toUpperCase())
  }
}

val up = new Upper
up.upper("foo", "bar", "baz").foreach(printf("%s ", _))

class Point(val x: Double, val y: Double) {
  override def toString: String = s"Point($x,$y)"
}

abstract class Shape() {
  def draw(): Unit
}

class Circle(val center: Point, val radius: Double) extends Shape {
  override def draw() = println(s"Circle draw: $this")

  override def toString: String = s"Circle($center,$radius)"
}

class Rectangle(val lowerLeft: Point, val height: Double, val width: Double) extends Shape {
  override def draw() = println(s"Rectangle.draw: $this")

  override def toString: String = s"Rectangle($lowerLeft, $height, $width)"
}

import scala.actors._
import scala.actors.Actor._
object ShapeDrawingActor extends Actor {
  def act(): Unit = {
    loop {
      receive {
        case s: Shape => s.draw()
        case "exit" => println("exiting..."); exit
        case x: Any => println("Error: Unknown message! " + x)
      }
    }
  }
}

ShapeDrawingActor.start()

ShapeDrawingActor ! new Circle(new Point(0.0, 0.0), 1.0)
ShapeDrawingActor ! new Rectangle(new Point(0.0, 0.0), 2, 5)

ShapeDrawingActor ! 3.234234
ShapeDrawingActor ! "exit"