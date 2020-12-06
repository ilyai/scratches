// Bridge
// There are a set of related objects using another set of objects. This second set represents an implementation of the first set.

abstract class Drawing {
  def drawLine()
  def drawCircle()
}

abstract class Shape(dp: Drawing) {
  def draw()
  def drawLine(): Unit = dp.drawLine()
  def drawCircle(): Unit = dp.drawCircle()
}

class Rectangle(dp: Drawing) extends Shape(dp) {
 def draw(): Unit = {
   drawLine()
   drawLine()
 }
}

class Circle(dp: Drawing) extends Shape(dp) {
  def draw(): Unit = {
    drawCircle()
  }
}

object DrawingImpl1 {
  def drawThisLine(): Unit = println("di1: drawing a line")
  def drawThisCircle(): Unit = println("di1: drawing a circle")
}

object DrawingImpl2 {
  def drawLine(): Unit = println("di2: drawing a line")
  def drawCircle(): Unit = println("di2: drawing a circle")
}

class DrawingV1 extends Drawing {
  def drawLine(): Unit = DrawingImpl1.drawThisLine()
  def drawCircle(): Unit = DrawingImpl1.drawThisCircle()
}

class DrawingV2 extends Drawing {
  def drawLine(): Unit = DrawingImpl2.drawLine()
  def drawCircle(): Unit = DrawingImpl2.drawCircle()
}

val dp1 = new DrawingV1
val r1 = new Rectangle(dp1)

val dp2 = new DrawingV2
val r2 = new Circle(dp2)

r1.draw()
r2.draw()