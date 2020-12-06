abstract class Context {
  def lookup(s: String): Boolean
  def assign(exp: VariableExp, b: Boolean): Unit
}

abstract class AbstractExpression {
  def interpret(ctx: Context)
}

class TerminalExpression extends AbstractExpression {
  override def interpret(ctx: Context): Unit = ???
}

class NonterminalExpression extends AbstractExpression {
  override def interpret(ctx: Context): Unit = ???
}

abstract class BooleanExp {
  def evaluate(ctx: Context): Boolean
//  def replace(s: String, exp: BooleanExp): BooleanExp
//  def copy(): BooleanExp
}

class VariableExp(val name: String) extends BooleanExp {
  override def evaluate(ctx: Context): Boolean = ctx.lookup(name)
//  override def copy(): BooleanExp = new VariableExp(name)
}

class AndExp(op1: BooleanExp, op2: BooleanExp) extends BooleanExp {
  override def evaluate(ctx: Context): Boolean = op1.evaluate(ctx) && op2.evaluate(ctx)
}

val ctx = new Context {
  val mapping = collection.mutable.HashMap[String, Boolean]()

  override def lookup(s: String): Boolean = mapping(s)

  override def assign(exp: VariableExp, b: Boolean): Unit = mapping.addOne(exp.name -> b)
}
val x = new VariableExp("x")
val y = new VariableExp("y")

val exp = new AndExp(x, y)

ctx.assign(x, true)
ctx.assign(y, false)

val result = exp.evaluate(ctx)