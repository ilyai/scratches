object HttpMethod extends Enumeration {
  type Method = Value
  val Connect, Delete, Get, Head, Options = Value
}

import HttpMethod._

def handle(method: HttpMethod.Method) = method match {
  case Connect => println(s"Connect: ${method.id}")
  case Delete => println(s"Delete: ${method.id}")
  case Get => println(s"Get: ${method.id}")
  case Head => println(s"Head: ${method.id}")
  case Options => println(s"Options: ${method.id}")
}

HttpMethod.values foreach { handle }

println( HttpMethod )

sealed abstract class HttpMethodV2(val id: Int) {
  def name = getClass.getSimpleName
  override def toString: String = name
}

case object ConnectV2 extends HttpMethodV2(0)
case object DeleteV2 extends HttpMethodV2(1)
case object GetV2 extends HttpMethodV2(2)

sealed abstract class HttpMethodV3()

case class ConnectV3(body: String) extends HttpMethodV3
case class DeleteV3(body: String) extends HttpMethodV3
case class GetV3(body: String) extends HttpMethodV3