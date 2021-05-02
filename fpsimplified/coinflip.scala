import scala.annotation.tailrec
import scala.util.Random

//type History = List[(Int,Int)]

case class GameState (numFlips: Int,
                      numCorrectGuesses: Int,
                      history: List[(Int,Int)])

object CoinFlipUtils {
  def showPrompt(): Unit = print("\n(h)eads, (t)ails, or (q)uit: ")
  def getUserInput() = io.StdIn.readLine().trim.toUpperCase()

  def printableFlipResult(flip: String) = flip match {
    case "H" => "Heads"
    case "T" => "Tails"
  }

  def printGameState(printableResult: String, gameState: GameState) = {
    print(s"Flip was $printableResult.")

  }

  def printGameState(numFlips: Int, numCorrectGuesses: Int) = {
    println(s"#Filps: ${numFlips}, #Correct: ${numCorrectGuesses}")
  }

  def printGameHistory(gameState: GameState) = {
    gameState.history.reverse.foreach {
      case (flips, guesses) => printGameState(flips, guesses)
    }
  }

  def printGameOver() = println("\n=== GAME OVER ===")

  def tossCoin(r: Random) = r.nextInt(2) match {
    case 0 => "H"
    case 1 => "T"
  }

  def saveHistory(gameState: GameState) = gameState.copy(
    history = (gameState.numFlips, gameState.numCorrectGuesses) :: gameState.history
  )
}

object CoinFlip extends App {
  import CoinFlipUtils._

  @tailrec
  def mainLoop(gameState: GameState, random: Random): Unit = {
    showPrompt()

    val userInput = getUserInput()

    userInput match {
      case "H" | "T" => {
        val coinTossResult = tossCoin(random)
        val newNumFlips = gameState.numFlips + 1

        val newCorrectGuesses = gameState.numCorrectGuesses +
          (if (userInput == coinTossResult) 1 else 0)

        val newGameState = gameState.copy(
          numFlips = newNumFlips,
          numCorrectGuesses = newCorrectGuesses
        )
        printGameState(printableFlipResult(coinTossResult), gameState)
        mainLoop(newGameState, random)
      }
      case "N" => newGame(saveHistory(gameState))
      case _ => {
        printGameOver()
        printGameHistory(saveHistory(gameState))
      }
    }
  }

  def newGame(gameState: GameState = GameState(0,0,Nil)) = {
    val r = new Random
    mainLoop(gameState.copy(numFlips = 0, numCorrectGuesses = 0), r)
  }

  newGame()
}