

import scala.collection.mutable

abstract class Observer {
  Customer.attach(this)

  def update(c: Customer)
}

object Customer {
  private val observers = mutable.Set[Observer]()

  def attach(o: Observer) {
    observers += o
  }

  def detach(o: Observer) {
    observers -= o
  }
}

class Customer {
  import Customer.observers

  def notifyObservers(): Unit = {
    observers.foreach(_.update(this))
  }

  def getState() = {}
  def setState() = {}
}

class WelcomeLetter extends Observer {
  override def update(c: Customer): Unit = {
    println(s"[letter] customer updated: $c")
  }
}

class AddressVerification extends Observer {
  override def update(c: Customer): Unit = {
    println(s"[address] customer updated: $c")
  }
}

val customer = new Customer
val wl = new WelcomeLetter
val av = new AddressVerification
customer.notifyObservers()