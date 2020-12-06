class Producer {
  def produce() = println("Producer is working hard")
  def meet() = println("Producer has time to meet you now")
}

class Proxy {
  var occupied = false
  var producer: Producer = _

  def produce() = {
    if (occupied) {
      println("Producer is busy!")
    } else {
      producer = new Producer
      producer.meet()
    }
  }
}

val p = new Proxy
p.produce()
p.occupied = true
p.produce()