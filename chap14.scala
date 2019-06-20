assert(2 == 2)
//assert(2 == 4)

import org.scalatest.prop.Checkers
import org.scalatest.{Suite, WordSpec}


class ElementSuite extends Suite {
  assert(2 == 2)
}

(new ElementSuite).execute()


import org.scalatest.FunSuite

class ElementSuite2 extends FunSuite {
  test("2 + 2 == 4") {
    assert(2 + 2 == 4)
  }
}

//import org.scalatest.FlatSpec
//import org.specs2.matcher.ShouldMatchers

//class ElementSpec extends FlatSpec with ShouldMatchers {
//  "multiplication" should "multiplicate two integers" in {
//    (2 * 2) shouldEqual 4
//  }
//}

//(new ElementSpec).execute()

class ElementSpec extends WordSpec with Checkers {
  "multiplication" must {
    "multiplicate two integers" in {
      check((x: Int) => x * x == 4)
    }
  }
}

(new ElementSpec).execute()