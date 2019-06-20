val cs = List('a', 'b', 'c')

class BankAccount {
  private var bal: Int = 0
  def balance: Int = bal
  def deposit(amount: Int): Unit = {
    require(amount > 0)
    bal += amount
  }
  def withdraw(amount: Int): Boolean =
    if (amount > bal) false
    else {
      bal -= amount
      true
    }
}

val account = new BankAccount
account.deposit(100)
account.withdraw(80)
account.withdraw(80)



class Keyed {
  def computeKey: Int = ???
}

class MemoKeyed extends Keyed {
  private var keyCache: Option[Int] = None

  override def computeKey: Int = {
    if (keyCache.isEmpty) keyCache = Some(super.computeKey)
    keyCache.get
  }
}

class STime {
  var hour = 12
  var minute = 0
}

val st = new STime
st.hour
st.minute

class Time {
  private[this] var h = 12
  private[this] var m = 0

  def hour: Int = h
  def hour_=(x: Int) { h = x }

  def minute: Int = m
  def minute_=(x: Int) { m = x }
}

class Thermometer {
  var celsius: Float = _
  def fahrenheit = celsius * 9 / 5 + 32
  def fahrenheit_=(f: Float): Unit = {
    celsius = (f - 32) * 5 / 9
  }

  override def toString: String = s"${fahrenheit}F/${celsius}C"
}

val t = new Thermometer
t.celsius = 100
t
t.fahrenheit = -40
t
