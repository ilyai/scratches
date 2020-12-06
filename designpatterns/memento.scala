class State

class Memento(private var state: State) {
  def getState: State = state
  def setState(s: State): Unit = state = s
}

class Originator {
  var state: State = _

  def setMemento(m: Memento): Unit = state = m.getState
  def createMemento = new Memento(state)
  def changeSomeState() = println("changing state")
}

class Caretaker {
  val originator = new Originator
  var state: Memento = _

  execute()
  unexecute()

  def execute() = {
    state = originator.createMemento
    originator.changeSomeState()
  }

  def unexecute() = {
    originator.setMemento(state)
    originator.changeSomeState()
  }
}

new Caretaker