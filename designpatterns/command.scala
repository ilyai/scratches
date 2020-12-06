abstract class Command {
  def execute()
}

class MacroCommand extends Command {
  private val commands = collection.mutable.Set[Command]()

  def add(c: Command): Unit = commands.add(c)
  def remove(c: Command): Unit = commands.remove(c)

  override def execute(): Unit = {
    for (c <- commands) c.execute()
  }
}

class Document {
  def paste() = println("pasting document")
}

class PasteCommand(receiver: Document) extends Command {
  override def execute(): Unit = receiver.paste()
}

class Invoker(command: Command) {
  def invoke() = command.execute()
}

class Client {
  val document = new Document
  val command = new PasteCommand(document)
  val invoker = new Invoker(command)
  invoker.invoke()
}

new Client