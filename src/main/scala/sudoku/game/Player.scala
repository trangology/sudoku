package sudoku.game

import scala.io.StdIn.readLine

sealed trait Player

case object Human extends Player {
  def pickDigit(): (String, String, String) = {
    println("Please enter row, col and digit line-by-line: ")
    val x, y, d = readLine()
    (x, y, d)
  }
}

case object Solver extends Player {
  def solve(puzzle: Puzzle, unsolvedCells: Vector[(Int, Int)]): Unit = {
    if (unsolvedCells.isEmpty) {
      Game(Solver, puzzle, SOLVED).play()
    }
    else {
      val (row, col) = (unsolvedCells.head._1, unsolvedCells.head._2)
      val validDigits = puzzle.getValidDigits(row, col)
      for (digit <- validDigits) {
        val updatedPuzzle = puzzle.applyChoice(row, col, digit)
        solve(updatedPuzzle, unsolvedCells.tail)
      }
    }
  }
}
