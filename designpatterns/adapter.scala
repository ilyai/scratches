// Adapter
// Normally don’t worry about interfaces here, so don’t usually think about it. However, if you know some existing code is going to be incorporated into your system, it is likely that an adapter will be needed since it is unlikely this pre-existing code will have the correct interface.

abstract class Shape {
  var location: String = _
  def setLocation(loc: String) = location = loc
  def getLocation = location
  def display()
}

class Point extends Shape {
  override def display(): Unit = println("displaying a point")
}

class Line extends Shape {
  override def display(): Unit = println("displaying a line")
}

class Square extends Shape {
  override def display(): Unit = println("displaying a square")
}

def drawShape(shape: Shape) = shape.display()

drawShape(new Line)
drawShape(new Square)

abstract class AnimalMovement
class AnimalFly extends AnimalMovement
class AnimalWalk extends AnimalMovement
