abstract class CalcTax {
  def taxAmount(itemSold: Any, quantity: Double, price: Double): Double
}

class USTax extends CalcTax {
  override def taxAmount(itemSold: Any, quantity: Double, price: Double): Double = price * 0.05 * quantity
}

class CanTax extends CalcTax {
  override def taxAmount(itemSold: Any, quantity: Double, price: Double): Double = price * 0.04 * quantity
}

object Country extends Enumeration {
  val US, Canada = Value
}

class SalesOrder(country: Country.Value) {
  def calcTax(itemSold: Any, quantity: Double, price: Double): Double = {
    val taxCalculator = country match {
      case Country.US => new USTax
      case Country.Canada => new CanTax
    }
    taxCalculator.taxAmount(itemSold, quantity, price)
  }
}

val so1 = new SalesOrder(Country.US)
so1.calcTax((), 1, 10)

val so2 = new SalesOrder(Country.Canada)
so2.calcTax((), 1, 10)