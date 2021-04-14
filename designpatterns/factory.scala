trait Drawing {
  def draw: Unit
}

trait CircleDrawing extends Drawing {
  override def draw: Unit = println(s"Circle.draw: $this")
}

object ShapeFactory {
  def makeShape(args: Any*) = args(0) match {
    case "circle" => ???
    case "rectangle" => ???
    case "triangle" => ???
    case x => throw new IllegalArgumentException(s"unknown: $x")
  }
}