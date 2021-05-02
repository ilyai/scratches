//case class State(value: Int) {
//  def flatMap(f: Int => State): State = State(f(value).value)
//  def map(f: Int => Int) = State(f(value))
//}
//
//for {
//  a <- State(20)
//  b <- State(a + 15)
//  c <- State(b + 0)
//} yield c


case class State[S,A](run: S => (S,A)) {
  def flatMap[B](f: A => State[S,B]): State[S,B] = State { (s0: S) =>
    val (s1, a) = run(s0)
    f(a).run(s1)
  }
  def map[B](f: A => B): State[S,B] = flatMap(a => State.point(f(a)))
}

object State {
  def point[S,A](v: A): State[S,A] = State(run = s => (s,v))
}

case class GolfState(distance: Int)

def swing(distance: Int): State[GolfState,Int] = State { (s: GolfState) =>
  val newDistance = s.distance + distance
  (GolfState(newDistance), newDistance)
}

val stateWIthNewDistance = for {
  _ <- swing(20)
  _ <- swing(15)
  total <- swing(0)
} yield total

val beginningState = GolfState(0)
val result: (GolfState, Int) = stateWIthNewDistance.run(beginningState)

type Stack = List[String]
def push(x: String): State[Stack, Unit] = State[Stack,Unit] {
  xs => (x :: xs, ())
}
def pop: State[Stack,String] = State[Stack,String] {
  xs => (xs.tail, xs.head)
}

(for {
  _ <- push("foo")
  _ <- push("bar")
  _ <- pop
  a <- pop
} yield a).run(Nil)._2

