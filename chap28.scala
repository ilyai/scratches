import scala.xml.NodeSeq

val a = <a>
  This is some XML.
  Here is a tag: <atag />
</a>

val b = <b>{ "Hello" + ", world" }</b>

val yearMade = 1955

val c = <c>
  { if (yearMade < 2000) <old>{ yearMade }</old> else NodeSeq.Empty }
</c>

val d = <a>{ "</a>potential security hole<a>" }</a>

abstract class CCTherm {
  val description: String
  val yearMade: Int

  override def toString = description

  def toXML =
    <cctherm>
      <description>{description}</description>
      <yearMade>{yearMade}</yearMade>
    </cctherm>
}

object CCTherm {
  def fromXML(node: scala.xml.Node): CCTherm =
    new CCTherm {
      override val description: String = (node \ "description").text
      override val yearMade: Int = (node \ "yearMade").text.toInt
    }
}

val therm = new CCTherm {
  override val description: String = "hot dog #5"
  override val yearMade: Int = 1952
}

val xml = therm.toXML
val cct = CCTherm.fromXML(xml)
cct

val e = <a>{{brace yourself}}</a>

a.text
c \ "old"
c \\ "old"

val joe =
  <employee
    name="Joe"
    rank="code monkey"
    serial="123"
    />

joe \ "@name"

scala.xml.XML.save("cctherm.xml", xml)
val loadnode = scala.xml.XML.loadFile("cctherm.xml")

def proc(node: scala.xml.Node): String =
  node match {
    case <a>{contents}</a> => s"It's an a: $contents"
    case <b>{contents}</b> => s"It's a b: $contents"
    case _ => "It's something else."
  }

def proc2(node: scala.xml.Node): String =
  node match {
    case <a>{contents @ _*}</a> => s"It's an a: $contents"
    case <b>{contents @ _*}</b> => s"It's a b: $contents"
    case _ => "It's something else."
  }

proc(<a>apple</a>)
proc(<b>banana</b>)
proc(<c>cherry</c>)

proc(<a>a <em>red</em> apple</a>)
proc2(<a>a <em>red</em> apple</a>)

val catalog =
  <catalog>
    <cctherm>
      <description>hot dog #5</description>
      <yearMade>1952</yearMade>
    </cctherm>
    <cctherm>
      <description>Sprite Boy</description>
      <yearMade>1964</yearMade>
    </cctherm>
  </catalog>

catalog match {
  case <catalog>{ therms @ _* }</catalog> =>
    for (therm @ <cctherm>{_*}</cctherm> <- therms)
      println("processing: " + (therm \ "description").text)
}