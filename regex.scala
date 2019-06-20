private val IpAddressRe = """.*?(\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}).*""".r

("x12.12.12.12x", "hnull") match {
  case (IpAddressRe(ip), _) => ip
  case _ => throw new Exception("Bad IP address format")
}

import org.joda.time.{DateTime, Duration, LocalDate, Period}
new DateTime(1509521033945L)