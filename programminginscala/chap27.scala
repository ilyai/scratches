@deprecated class QuickAndDirty {
  // ....
}

//@cool val normal = "Hello"
//@coolerThan(normal) val fonzy = "heeyyy"

import annotation.{unchecked, _}

class strategy(arg: Annotation) extends Annotation

class delayed extends Annotation

@strategy(new delayed) def f(){}

@deprecated("use newShinyMethod() instead")
def bigMistake() = ???

class Foo {
  @transient
  val bar: Any = "baar"

//  @scala.reflect.BeanProperty
  var crazy = "crazzy"

//  @tailrec
//  def recursive = ???

  @unchecked
  def select(b: Any) = b match {
    case s: String => s
    case _ => b
  }

  @native
  def beginCountDown() { }
}


