type Request = Int

class Handler(successor: Handler) {
  def tryHandle(request: Request): Unit = {
    val handled = handle(request)
    if (!handled) {
      successor.tryHandle(request)
    }
  }

  def handle(request: Request): Boolean = {
    throw new NotImplementedError("Must provide implementation in subclass!")
  }
}

class ConcreteHandler1(successor: Handler) extends Handler(successor) {
  override def handle(request: Request): Boolean = {
    if (request > 0 && request <= 10) {
      println(s"Request ${request} handled in handler1")
      return true
    }
    false
  }
}

class DefaultHandler(successor: Handler) extends Handler(successor) {
  override def handle(request: Request): Boolean = {
    println(s"End of chain, no handler for ${request}")
    true
  }
}

class Client {
  val handler = new ConcreteHandler1(new DefaultHandler(null))

  def delegate(requests: Seq[Request]): Unit = {
    for (request <- requests) {
      handler.tryHandle(request)
    }
  }
}

val c = new Client
val requests = Seq(1,2,30)
c.delegate(requests)