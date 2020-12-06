abstract class DisplayDriver {
  def draw()
}
abstract class PrintDriver {
  def print()
}

class Lrdd extends DisplayDriver {
  def draw() = println("Drawing with LRDD")
}
class Hrdd extends DisplayDriver {
  def draw() = println("Drawing with HRDD")
}

class Lrdp extends PrintDriver {
  def print() = println("Printing with LRDP")
}
class Hrdp extends PrintDriver {
  def print = println("Printing with HRDP")
}

abstract class ResourceFactory {
  def getDisplayDriver: DisplayDriver
  def getPrintDriver: PrintDriver
}

class LowResourceFactory extends ResourceFactory {
  override def getDisplayDriver = new Lrdd
  override def getPrintDriver = new Lrdp
}

class HighResourceFactory extends ResourceFactory {
  override def getDisplayDriver = new Hrdd
  override def getPrintDriver = new Hrdp
}

class AppControl(factory: ResourceFactory) {
  private val dd = factory.getDisplayDriver
  private val pd = factory.getPrintDriver

  def draw() = dd.draw()
  def print() = pd.print()
}

val ac1 = new AppControl(new LowResourceFactory)
ac1.draw()
ac1.print()

val ac2 = new AppControl(new HighResourceFactory)
ac2.draw()
ac2.print()