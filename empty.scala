case class Foo(s: String)
var map: Map[String, Foo] = Map.empty

map = map + ("hi" -> Foo("one"))
map = map + ("hi" -> Foo("two"))

map