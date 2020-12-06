class Maze
class Wall {
  override def clone = new Wall
}
class Room
class Door {
  override def clone = new Door
  def initialize(r1: Room, r2: Room) {}
}

abstract class MazeFactory {
  def makeWall: Wall
  def makeDoor(r1: Room, r2: Room): Door
}

class MazePrototypeFactory(m: Maze, w: Wall, r: Room, d: Door) extends MazeFactory {
  def makeWall = w.clone()
  def makeDoor(r1: Room, r2: Room) = {
    val door = d.clone()
    door.initialize(r1, r2)
    door
  }
}

class MazeGame {
  def createMaze(factory: MazeFactory): Unit = {
    println(s"Creating maze with wall (${factory.makeWall}) and door (${factory.makeDoor(new Room, new Room)})")
  }
}

val simplePrototypeFactory = new MazePrototypeFactory(new Maze, new Wall, new Room, new Door)
val game = new MazeGame
game.createMaze(simplePrototypeFactory)
