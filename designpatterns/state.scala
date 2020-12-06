class Context(state: State) {
  def request() = state.handle()
}

abstract class State {
  def handle()
}

class ConcreteStateA extends State {
  override def handle(): Unit = ???
}
class ConcreteStateB extends State {
  override def handle(): Unit = ???
}


abstract class TCPState {
  def open(t: TCPConnection): Unit
  def close(t: TCPConnection): Unit
  def ack(t: TCPConnection): Unit
  def changeState(t: TCPConnection, s: TCPState): Unit = {
    println(s"changing state to ${s.getClass.getSimpleName}")
    t.changeState(s)
  }
}

class TCPEstablished extends TCPState {
  override def open(t: TCPConnection): Unit = println("E open")
  override def close(t: TCPConnection): Unit = println("E close")
  override def ack(t: TCPConnection): Unit = println("E ack")
}

class TCPListen extends TCPState {
  override def open(t: TCPConnection): Unit = println("L open")
  override def close(t: TCPConnection): Unit = println("L close")
  override def ack(t: TCPConnection): Unit = println("L ack")
}

class TCPClosed extends TCPState {
  override def open(t: TCPConnection): Unit = {
    println("C open")
    changeState(t, new TCPListen)
  }
  override def close(t: TCPConnection): Unit = {
    println("C close")
    changeState(t, new TCPListen)
  }
  override def ack(t: TCPConnection): Unit = {
    println("C ack")
  }
}

class TCPConnection(private var state: TCPState = new TCPClosed) {
  def open(): Unit = state.open(this)
  def close(): Unit = state.close(this)
  def ack(): Unit = state.ack(this)
  def changeState(s: TCPState)  = state = s
}

val t = new TCPConnection()
t.open()
t.ack()
t.close()
t.ack()