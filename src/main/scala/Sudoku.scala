import sudoku.game._


/**
 * @author https://github.com/trangology
 */

object Sudoku {
  def main(args: Array[String]): Unit = {
    val puzzle = new Puzzle()
    Game(Human, puzzle, GET_STARTED).play()
    Game(Solver, puzzle, GET_STARTED).play()
  }
}