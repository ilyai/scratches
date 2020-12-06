class Maze {
  def addRoom(): Unit = {
    println("adding room")
  }
}

abstract class MazeBuilder {
  def buildMaze()
  def buildRoom(room: Int)
  def buildDoor(roomFrom: Int, roomTo: Int)
  def getMaze: Maze
}

class StandardMazeBuilder extends MazeBuilder {
  var currentMaze: Maze = _

  def buildMaze() = {
    println("building maze")
    currentMaze = new Maze
  }
  def buildRoom(room: Int) = println(s"building room $room")
  def buildDoor(roomFrom: Int, roomTo: Int) = println(s"building door from $roomFrom to $roomTo")
  def getMaze: Maze = currentMaze
}

class MazeGame {
  def createMaze(builder: MazeBuilder): Maze = {
    builder.buildMaze()
    builder.buildRoom(1)
    builder.buildRoom(2)
    builder.buildDoor(1, 2)
    builder.getMaze
  }

  def createComplexMaze(builder: MazeBuilder): Maze = {
    builder.buildMaze()
    builder.buildRoom(1)
    builder.buildRoom(2)
    // ...
    builder.buildRoom(1001)
    builder.getMaze
  }
}

val game = new MazeGame
val m1 = game.createMaze(new StandardMazeBuilder)
val m2 = game.createComplexMaze(new StandardMazeBuilder)
