// Facade
// A complex system will be used which will likely not be utilized to its full extent.

class Private {
  var i = 5

  def get() = println(s"current value: ${i}")
  def set(v: Int) = this.i = v
  def run() = println("running")
  def jump() = println("jumping")
}

class Facade {
  val p = new Private
  def perform(v: Int, doRun: Boolean = false): Unit = {
    p.set(v)
    p.get()
    if (doRun) {
      p.run()
    }
  }
}

val facade = new Facade
facade.perform(10, true)