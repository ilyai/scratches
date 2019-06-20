abstract class Element {
  def contents: Array[String]
  def height: Int = contents.length
  def width: Int = if (height == 0) 0 else contents(0).length
  def above(that: Element): Element =
    new ArrayElement(this.contents ++ that.contents)
  def beside(that: Element): Element = {
    val contents = new Array[String](this.contents.length)
    for (i <- 0 until this.contents.length)
      contents(i) = this.contents(i) + that.contents(i)
    new ArrayElement(contents)
  }

  override def toString: String = contents mkString "\n"
}

object Element {
  private class ArrayElement(conts: Array[String]) extends Element {
    def contents: Array[String] = conts
  }
  private class LineElement(s: String) extends Element {
    val contents = Array(s)
    override def width = s.length
    override def height = 1
  }
  def elem(contents: Array[String]): Element =
    new ArrayElement(contents)
  def elem(chr: Char, width: Int, height: Int): Element =
    new UniformElement(chr, width, height)
  def elem(line: String): Element =
    new LineElement(line)
}

class ArrayElement(conts: Array[String]) extends Element {
  def contents: Array[String] = conts
}

val ae = new ArrayElement(Array("hello", "world"))
ae.width

val e: Element = new ArrayElement(Array("hello"))

class ArrayElement2(
                  val contents: Array[String]
                  ) extends Element


class ArrayElement3(x123: Array[String]) extends Element {
  val contents: Array[String] = x123
}

class Cat {
  val dangerous = false
}

class Tiger (
            override val dangerous: Boolean,
            private var age: Int
            ) extends Cat

class Tiger2(param1: Boolean, param2: Int) extends Cat {
  override val dangerous: Boolean = param1
  private var age = param2
}

class LineElement(s: String) extends ArrayElement(Array(s)) {
  override def width = s.length
  override def height = 1
}

class UniformElement(
                    ch: Char,
                    override val width: Int,
                    override val height: Int
                    ) extends Element {
  private val line = ch.toString * width
  def contents = Array.fill(height)(line)
}

val e1: Element = new ArrayElement(Array("hello", "world"))
val ae2: ArrayElement = new LineElement("hello")
val e2: Element = ae2
val e3: Element = new UniformElement('x', 2, 3)

abstract final class ArrayElement4 extends Element {
  def demo(): Unit = {
    println("ArrayElement's implementation invoked")
  }
}

class LineElement2(s: String) extends Element {
  val contents = Array(s)
  override def width = s.length
  override def height = 1
}

//new ArrayElement(
//  for (
//    (line1, line2) <- this.contents zip that.contents
//  ) yield line1 + line2
//)

Array(1,2,3) zip Array("a", "b")

