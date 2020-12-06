abstract class Component {
  def printTicket()
}

class SalesTicket extends Component {
  override def printTicket(): Unit = {
    println("< sales ticket printing >")
  }
}

class TicketDecorator(component: Component) extends Component {
  override def printTicket(): Unit = {
    component.printTicket()
  }
}

class HeaderDecorator(component: Component) extends TicketDecorator(component) {
  override def printTicket(): Unit = {
    printHeader()
    super.printTicket()
  }
  private def printHeader(): Unit = println("header")
}

class FooterDecorator(component: Component) extends TicketDecorator(component) {
  override def printTicket(): Unit = {
    super.printTicket()
    printFooter()
  }
  private def printFooter(): Unit = println("footer")
}

val st = new HeaderDecorator(new FooterDecorator(new SalesTicket))
st.printTicket()