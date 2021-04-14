trait AbstractSubject {
  type Observer

  private var observers = List[Observer]()
  def addObserver(observer: Observer) = observers ::= observer
  def notifyObservers = observers foreach notify
  def notify(observer: Observer): Unit
}

trait SubjectForReceiveUpdateObservers extends AbstractSubject {
  override type Observer = { def receiveUpdate(subject: Any) }

  override def notify(observer: Observer): Unit = observer.receiveUpdate(this)
}

trait SubjectFromFunctionalObservers extends AbstractSubject {
  override type Observer = AbstractSubject => Unit

  override def notify(observer: Observer): Unit = observer(this)
}

abstract class SubjectObserver {
  type S <: Subject
  type O <: Observer

  trait Subject {
    self: S =>
    private var observers = List[O]()
    def addObservers(observer: O) = observers ::= observer
    def notifyObservers = observers.foreach(_.receiveUpdate(self))
  }

  trait Observer {
    def receiveUpdate(subject: S)
  }
}

abstract class Button {
  def click()
}

object ButtonSubjectObserver extends SubjectObserver {
  type S = ObservableButton
  type O = ButtonObserver

  class ObservableButton(val name: String)  extends Button with Subject {
    def click() = {
      println("click()")
      notifyObservers
    }
  }

  trait ButtonObserver extends Observer {
    def receiveUpdate(subject: ObservableButton)
  }
}

class ButtonClickObserver extends ButtonSubjectObserver.ButtonObserver {
  val clicks = collection.mutable.HashMap[String,Int]()

  override def receiveUpdate(button: ButtonSubjectObserver.ObservableButton): Unit = {
    val count = clicks.getOrElse(button.name, 0) + 1
    clicks.update(button.name, count)
  }
}

def clickButton(button: Button, nClicks: Int) =
  for (i <- 1 to nClicks) button.click()

val button1 = new ButtonSubjectObserver.ObservableButton("button1")
val button2 = new ButtonSubjectObserver.ObservableButton("button2")
val buttonObserver = new ButtonClickObserver
button1.addObservers(buttonObserver)
button2.addObservers(buttonObserver)
clickButton(button1, 1)
clickButton(button2, 2)
buttonObserver.clicks("button1")
buttonObserver.clicks("button2")
