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


trait Clickable {
  private var clicks = 0
  def count = clicks

  def click() = { clicks += 1 }
}

class Widget
class Button(val label: String) extends Widget with Clickable {
  override def click(): Unit = {
    super.click()
    println("click!")
  }
}

trait VetoableClicks extends Clickable {
  val maxAllowed = 2

  abstract override def click(): Unit = {
    if (count < maxAllowed) super.click()
  }
}

val button1 = new Button("click me!")
for (_ <- 1 to 3) button1.click()

val button2 = new Button("click me!") with VetoableClicks
for (_ <- 1 to 3) button2.click()