case class CreditCard()
class Coffee(val price: Int = 5)

case class Charge(cc: CreditCard, amount: Double) {
  def combine(other: Charge): Charge = {
    require(cc == other.cc, "Cannot combine charges to different cards")
    Charge(cc, amount + other.amount)
  }

  override def toString: String = s"<Charge $$$amount>"
}

def coalesce(charges: List[Charge]): List[Charge] = {
  charges.groupBy(_.cc).values.map(_.reduce(_ combine _)).toList
}

class Cafe {
  def buyCoffee(cc: CreditCard): (Coffee, Charge) = {
    val cup = new Coffee()
    (cup, Charge(cc, cup.price))
  }

  def buyCoffees(cc: CreditCard, n: Int): (List[Coffee], Charge) = {
    val purchases: List[(Coffee, Charge)] = List.fill(n)(buyCoffee(cc))
    val (coffees, charges) = purchases.unzip
    (coffees, charges.reduce((c1,c2) => c1.combine(c2)))
  }
}

val myCafe = new Cafe()
myCafe.buyCoffee(CreditCard())
val (coffees, charge) = myCafe.buyCoffees(CreditCard(), 10)
println(s"Bought ${coffees.size} coffees for ${charge.amount} USD")


val x = "Hello, World"
val r1 = x.reverse
val r2 = x.reverse

val x2 = new StringBuilder("Hello")
val y = x2.append(", World")
val r3 = y.toString()
val r4 = y.toString()

val x3 = new StringBuilder("Hello")
val r5 = x3.append(", World").toString
val r6 = x3.append(", World").toString

