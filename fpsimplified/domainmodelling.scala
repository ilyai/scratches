type Money = BigDecimal

sealed trait Topping
case object Cheese extends Topping
case object Pepperoni extends Topping

sealed class CrustSize
case object SmallCrustSize extends CrustSize
case object LargeCrustSize extends CrustSize

sealed trait CrustType
case object ThinCrustType extends CrustType
case object ThickCrustType extends CrustType


/*
class Pizza {
  var crustSize = SmallCrustSize
  var crustType = ThinCrustType

  val toppings = collection.mutable.ArrayBuffer[Topping]()

  def addTopping(t: Topping) { toppings += t }
  def removeTopping(t: Topping) { toppings -= t }
  def removeAllToppings() { toppings.clear() }

  def getPrice(toppingsPrices: Map[Topping, Money],
               crustSizePrices: Map[CrustSize, Money],
               crustTypePrices: Map[CrustType, Money]): Money = ???
}

class Order {
  class Address (
                var street: String,
                var city: String,
                var state: String
                )

  class Customer (
                 var name: String,
                 var phone: String,
                 var address: Address
                 )

  val pizzas = collection.mutable.ArrayBuffer[Pizza]()

  var customer: Customer = null

  def addPizza(p: Pizza) { pizzas += p }
  def removePizza(p: Pizza) { pizzas -= p }

  def getBasePrice: Money  = ???
  def getTaxes: Money = ???
  def getTotalPrice: Money = ???
}*/

case class Pizza(crustSize: CrustSize,
                 crustType: CrustType,
                 toppings: Seq[Topping])

case class Address(street: String,
                   city: String,
                   state: String)

case class Customer(name: String,
                    phone: String,
                    address: Address)

case class Order(pizzas: Seq[Pizza],
                 customer: Customer)

object PizzaService {
  def addTopping(p: Pizza, t: Topping): Pizza = ???
  // ...
}

//object Pizza {
//  def addTopping(p: Pizza, t: Topping): Pizza = ???
//  // ...
//}

type Color = java.awt.Color
trait Animal

abstract class AnimalWithTail(tailColor: Color) extends Animal

trait DogTailServices {
  self: AnimalWithTail =>
  def wagTail = ???
  def lowerTail = ???
}

trait DogMouthServices {
  self: AnimalWithTail =>
  def bark = ???
  def lick = ???
}


object IrishSetter extends AnimalWithTail(java.awt.Color.RED)
  with DogTailServices with DogMouthServices

trait PizzaServiceInterface {
  def addTopping(p: Pizza, t: Topping): Pizza
  // ...
}

trait PizzaService extends PizzaServiceInterface {
  override def addTopping(p: Pizza, t: Topping): Pizza =
    p.copy(toppings = p.toppings :+ t)
}

//object PizzaService extends PizzaService
//import PizzaService._

trait PizzaDaoInterface {
  def getToppingPrices(): Map[Topping, Money]
}

object MockPizzaDao extends PizzaDaoInterface {
  override def getToppingPrices(): Map[Topping, Money] =
    Map(
      Cheese -> BigDecimal(1),
      Pepperoni -> BigDecimal(1)
    )
}

trait OrderServiceInterface {
  protected def database: PizzaDaoInterface

  def calculateOrderPrice(o: Order): Money
}

trait AbstractOrderService extends OrderServiceInterface {
  object PizzaService extends PizzaService

  private lazy val toppingPriceMap = database.getToppingPrices()

  override def calculateOrderPrice(o: Order): Money = ???
}

object MockDbOrderService extends AbstractOrderService {
  val database = MockPizzaDao
}

// Functional object
case class Pizza(
                crustSize: CrustSize,
                crustType: CrustType,
                val toppings: Seq[Topping]
                ) {
  def addTopping(t: Topping): Pizza =
    this.copy(toppings = this.toppings :+ t)
}

