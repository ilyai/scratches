abstract class DialogDirector {
//  def showDialog
  def createWidgets()
  def widgetChanged(w: Widget)
}

abstract class Widget(director: DialogDirector) {
  def changed() = director.widgetChanged(this)
}

class ListBox(director: DialogDirector) extends Widget(director) {
  def getSelection = "option1"
  def handleSelection() = {
    changed()
  }
}
class EntryField(director: DialogDirector) extends Widget(director) {
  private var text: String = _
  def setText(text: String): Unit = this.text = text
  def getText = text
}

class FontDialogDirector extends DialogDirector {
  var fontList: ListBox = _
  var fontName: EntryField = _

  override def createWidgets(): Unit = {
    fontList = new ListBox(this)
    fontName = new EntryField(this)
  }

  override def widgetChanged(changedWidget: Widget): Unit = {
    if (changedWidget == fontList) {
      fontName.setText(fontList.getSelection)
    } else {
      // ...
    }
  }
}

val mediator = new FontDialogDirector
mediator.createWidgets()
mediator.fontName.getText
mediator.fontList.handleSelection()
mediator.fontName.getText
