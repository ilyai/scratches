case class Player(name: String, score: Int)

def winner(p1: Player, p2: Player): Player = if (p1.score > p2.score) p1 else p2
def printWinner(p: Player) =  println(s"${p.name} is the winner")
def declareWinner(p1: Player, p2: Player) = printWinner(winner(p1, p2))

val players = List(
  Player("Sue", 7),
  Player("Bob", 8),
  Player("Joe", 4)
)

val p = players.reduceLeft(winner)
printWinner(p)
