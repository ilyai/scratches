abstract class Visitor {
  def visitConcreteElementA(e: ConcreteElementA)
  def visitConcreteElementB(e: ConcreteElementB)
}

class ConcreteVisitor1 extends Visitor {
  override def visitConcreteElementA(e: ConcreteElementA): Unit = {
    println(s"1. visiting ${e.getClass.getSimpleName}")
    e.operationA()
  }
  override def visitConcreteElementB(e: ConcreteElementB): Unit = {
    println(s"1. visiting ${e.getClass.getSimpleName}")
    e.operationB()
  }
}

class ConcreteVisitor2 extends Visitor {
  override def visitConcreteElementA(e: ConcreteElementA): Unit = {
    println(s"2. visiting ${e.getClass.getSimpleName}")
    e.operationA()
  }
  override def visitConcreteElementB(e: ConcreteElementB): Unit = {
    println(s"2. visiting ${e.getClass.getSimpleName}")
    e.operationB()
  }
}

abstract class Element {
  def accept(v: Visitor)
}

class ConcreteElementA extends Element {
  override def accept(v: Visitor): Unit = v.visitConcreteElementA(this)
  def operationA() = println("operation A")
}

class ConcreteElementB extends Element {
  override def accept(v: Visitor): Unit = v.visitConcreteElementB(this)
  def operationB() = println("operation B")
}

val visitor1 = new ConcreteVisitor1
val visitor2 = new ConcreteVisitor2
val a = new ConcreteElementA
val b = new ConcreteElementB

a.accept(visitor1)
b.accept(visitor1)
a.accept(visitor2)
b.accept(visitor2)



trait ShapeVisitor {
  def visit(circle: Circle): Unit
  def visit(rect: Rectangle): Unit
  def visit(tri: Triangle): Unit
}

class ShapeDrawingVisitor extends ShapeVisitor {
  override def visit(circle: Circle): Unit = println(s"Circle.draw: $circle")
  override def visit(rect: Rectangle): Unit = println(s"Rectangle.draw: $rect")
  override def visit(tri: Triangle): Unit = println(s"Triangle.draw: $tri")
}

case class Point(x: Double, y: Double)

sealed abstract class Shape {
//  def draw(): Unit
  def accept(visitor: ShapeVisitor): Unit
}

case class Circle(center: Point, radius: Double) extends Shape {
//  override def draw(): Unit = println(s"Circle.draw(): $this")
  override def accept(visitor: ShapeVisitor): Unit = visitor.visit(this)
}

case class Rectangle(lowerLeft: Point, height: Double, width: Double) extends Shape {
//  override def draw(): Unit = println(s"Rectangle.draw(): $this")
  override def accept(visitor: ShapeVisitor): Unit = visitor.visit(this)
}

case class Triangle(point1: Point, point2: Point, point3: Point) extends Shape {
//  override def draw(): Unit = println(s"Triangle.show(): $this")
  override def accept(visitor: ShapeVisitor): Unit = visitor.visit(this)
}

val p00 = Point(0, 0)
val p10 = Point(1, 0)
val p01 = Point(0, 1)

val shapes = Seq(
  Circle(p00, 5),
  Rectangle(p00, 2, 3),
  Triangle(p00, p10, p01)
)

val shapesDrawer = new ShapeDrawingVisitor
shapes foreach { _.accept(shapesDrawer) }

class ShapeDrawer(val shape: Shape) {
  def draw = shape match {
    case c: Circle => println(s"Circle.draw: $c")
    case r: Rectangle => println(s"Rectangle.draw: $r")
    case t: Triangle => println(s"Triangle.draw: $t")
  }
}

object ShapeDrawer {
  implicit def shape2ShapeDrawer(shape: Shape) = new ShapeDrawer(shape)
}

import ShapeDrawer._
shapes foreach { _.draw }