package sudoku.game

import sudoku.utils.*

/**
 * @param player the type of player, either Human or Solver.
 * @param puzzle the puzzle that contains a grid of 2-dimensional list of integers.
 * @param state the current state of the game, may be `GET_STARTED`, `UNSOLVED`, `UNSOLVABLE`, or `SOLVED`.
 */
case class Game(player: Player, puzzle: Puzzle, state: State) {

  def play(): Unit = {
    this.state match {
      case GET_STARTED => Game(this.player, puzzle, UNSOLVED).play()
      case UNSOLVED =>
        this.player match {
          case Human =>
            if (this.puzzle.isSolvable) {
              this.startGameForHuman(this.puzzle)
            } else {
              Game(this.player, puzzle, UNSOLVABLE).play()
            }
          case Solver => this.startGameForSolver(this.puzzle)
        }
      case SOLVED => printResult(SOLVED)
      case UNSOLVABLE => printResult(UNSOLVABLE)
    }
  }

  private def startGameForHuman(puzzle: Puzzle): Unit = {
    println("Rendering the puzzle...")
    Thread.sleep(1500)
    println(renderSudoku(this.puzzle.grid))
    println("NOTE: You can always stop playing the game by pressing `Ctrl + F2`.")
    val userInput = Human.pickDigit()

    puzzle.isValidInput(userInput) match {
      case Right(true) =>
        val choice = (userInput._1.toInt, userInput._2.toInt - 1, userInput._3.toInt)
        val updatedPuzzle = puzzle.applyChoice(choice)
        println("\nYour choice has been applied.\n")

        if (updatedPuzzle.getUnsolvedCells.isEmpty) {
          Game(Human, updatedPuzzle, SOLVED).play()
        } else {
          Game(Human, updatedPuzzle, UNSOLVED).play()
        }
      case _ => Game(Human, this.puzzle, UNSOLVED).play()
    }
  }

  private def startGameForSolver(puzzle: Puzzle): Unit = {
    val unsolvedCells = puzzle.getUnsolvedCells.toVector
    Solver.solve(puzzle, unsolvedCells)
  }

  /**
   * @param state either `UNSOLVABLE` or `SOLVED`.
   */
  def printResult(state: State): Unit = {
    this.player match {
      case Human =>
        state match {
          case UNSOLVABLE => println("Sorry, the puzzle now is unsolvable")
          case SOLVED =>
            println("Congrats, you have just solved the puzzle!\n")
            println(renderSudoku(this.puzzle.grid))
        }
      case Solver =>
        state match {
          case UNSOLVABLE => println("Hm, looks like my computation was not implemented properly.")
          case SOLVED =>
            println("Below is the solution to your puzzle, solved by my computation.")
            println(renderSudoku(this.puzzle.grid))
        }
    }
  }

}
