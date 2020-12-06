class Rational(n: Int, d: Int) {
//  println(s"Created $n/$d")

  require(d != 0)

  private val g = gcd(n.abs, d.abs)
  val numer: Int = n / g
  val denom: Int = d / g

  def this(n: Int) = this(n, 1)

  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)

  override def toString: String = s"$numer/$denom"

  def + (that: Rational): Rational =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom)

  def - (i: Int): Rational =
    new Rational(numer - i * denom, denom)

  def lessThan(that: Rational) =
    this.numer * that.denom < that.numer * this.denom

  def max(that: Rational) =
    if (this lessThan that) that else this
}

val x = new Rational(1,3)
val y = new Rational(5,7)
//val z = new Rational(5,0)

val sum = x + y

val r = new Rational(66,42)

implicit def intToRational(x: Int): Rational = new Rational(x)

2 + x