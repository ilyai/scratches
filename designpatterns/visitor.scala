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