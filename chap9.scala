object FileMatcher {
  private def filesHere = (new java.io.File(".")).listFiles

  def filesEnding(query: String) =
    for {
      file <- filesHere
      if file.getName.endsWith(query)
    } yield file

  def filesEnding2(query: String) =
    filesMatching(query, _.endsWith(_))

  def filesMatching(query: String, matcher: (String, String) => Boolean) = {
    for {
      file <- filesHere
      if matcher(file.getName, query)
    } yield file
  }
}

def containsNeg(nums: List[Int]): Boolean = {
  var exists = false
  for (num <- nums)
    if (num < 0)
      exists = true
  exists
}

def containsNeg2(nums: List[Int]) = nums.exists(_ < 0)

containsNeg(List(1,2,3))
containsNeg2(List(1,2,3))
containsNeg(List(1,2,-3))
containsNeg2(List(1,2,-3))
containsNeg2(Nil)

def planOldSum(x: Int, y: Int) = x + y

def curriedSum(x: Int)(y: Int) = x + y

curriedSum(1)(2)

val onePlus = curriedSum(1) _
onePlus(2)

println { "Hello, world!" }

var assertionsEnabled = true

def myAssert(predicate: () => Boolean) =
  if (assertionsEnabled && !predicate())
    throw new AssertionError

def byNameAssert(predicate: => Boolean) =
  if (assertionsEnabled && !predicate)
    throw new AssertionError

def boolAssert(predicate: Boolean) =
  if (assertionsEnabled && !predicate)
    throw new AssertionError

boolAssert(5 > 3)

assertionsEnabled = false

val x = 1


byNameAssert(x / 0 == 0)

// boolAssert(x / 0 == 0)

