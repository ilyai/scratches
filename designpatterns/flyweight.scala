class Glyph {
  def setFont(font: String, gc: GlyphContext) {}
}
class Window {}

class GlyphContext {
  var font: String = _

  def getFont = font
  def setFont(font: String) = this.font = font
}

class Character extends Glyph {
  def draw(w: Window, gc: GlyphContext) {}
}