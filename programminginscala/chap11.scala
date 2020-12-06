val a: AnyVal = 1
val b: AnyRef = Some(1)

42 hashCode

42 equals 42

42 max 43

-3 abs


val x = new String("abc")
val y = new String("abc")

x == y
x eq y
x ne y

def error(message: String): Nothing =
  throw new RuntimeException(message)

