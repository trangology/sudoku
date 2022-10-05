package sudoku.game

import sudoku.utils._


class Puzzle(val grid: List[List[Int]]) {

  def this() = this(
    List(
      List(3, 1, 6, 5, 7, 8, 4, 9, 2),
      List(5, 2, 9, 1, 3, 4, 7, 6, 8),
      List(4, 8, 7, 6, 2, 9, 5, 3, 1),

      List(2, 6, 3, 0, 1, 0, 0, 8, 0),
      List(9, 7, 4, 8, 6, 3, 0, 0, 5),
      List(8, 5, 1, 0, 9, 0, 6, 0, 0),

      List(1, 3, 0, 0, 0, 0, 2, 5, 0),
      List(0, 0, 0, 0, 0, 0, 0, 7, 4),
      List(0, 0, 5, 2, 0, 6, 3, 0, 0)
    )
  )


  private def generate: List[List[Int]] = {
    //  TODO: Algorithm for generating easy/medium puzzles?
    ???
  }

  private def getDigitsInBlock(row: Int, col: Int): List[Int] = {
    val startRow = row / 3 * 3
    val startCol = col / 3 * 3
    val digits = for (i <- startRow to startRow + 2;
                      j <- startCol to startCol + 2)
    yield this.grid(i)(j)
    digits.toList
  }

  private def hasUnknownCharacter(row: String, col: String, digit:String): Either[UserErrors, Boolean] = {
    if (row.toIntOption.isEmpty || col.toIntOption.isEmpty || digit.toIntOption.isEmpty) {
      println(UnknownCharacter().errorMessage)
      Left(UnknownCharacter())
    }
    else Right(false)
  }

  private def isValidCell(row: Int, col: Int, digit: Int): Either[UserErrors, Boolean] = {
    val unsolvedCells = getUnsolvedCells

    if ((row < 0 || row > 8) && (col < 0 || col > 8)) {
      println(IllegalCell(row, col, unsolvedCells).errorMessage)
      Left(IllegalCell(row, col, unsolvedCells))
    }

    else if (row < 0 || row > 8) {
      val validRows = unsolvedCells.map(_._1).distinct
      println(IllegalRow(row, validRows).errorMessage)
      Left(IllegalRow(row, validRows))
    }

    else if (col < 0 || col > 8) {
      val validColumns = unsolvedCells.filter(_._1 == row).map(_._2).distinct
      if (validColumns.isEmpty) println(s"There is no valid columns for row $row.")
      else println(IllegalCol(row, col, validColumns).errorMessage)
      Left(IllegalCol(row, col, validColumns))
    }

    else if (digit < 1 || digit > 9) {
      println(IllegalDigit(digit).errorMessage)
      Left(IllegalDigit(digit))
    }

    else if (!unsolvedCells.contains((row, col))) {
      println(NotUnsolvedCell(row, col, unsolvedCells).errorMessage)
      Left(NotUnsolvedCell(row, col, unsolvedCells))
    }

    else if (!getValidDigits(row, col).contains(digit)) {
      val validDigits = getValidDigits(row, col)
      println(InvalidDigit(digit, validDigits).errorMessage)
      Left(InvalidDigit(digit, validDigits))
    }

    else Right(true)
  }

  def getUnsolvedCells: List[(Int, Int)] = {
    val unsolvedCell = for (row <- 0 to 8; col <- 0 to 8 if this.grid(row)(col) == 0) yield (row, col)
    unsolvedCell.toList
  }

  def getValidDigits(row: Int, col: Int): List[Int] = {
    val digitsInCol = (0 to 8).map(this.grid(row)(_)).toList
    val digitsInRow = (0 to 8).map(this.grid(_)(col)).toList
    val digitsInBlock = this.getDigitsInBlock(row, col)
    val invalidDigits = (digitsInCol ::: digitsInRow ::: digitsInBlock).distinct
    (1 to 9).filterNot(invalidDigits.contains).toList
  }

  def isValidInput(input: (String, String, String)): Either[UserErrors, Boolean] = {
    val (row, col, digit) = (input._1, input._2, input._3)
    hasUnknownCharacter(row, col, digit) match {
      case Right(false) => isValidCell(row.toInt, col.toInt - 1, digit.toInt)
      case _ => Right(false)
    }
  }

  def isSolvable: Boolean = {
    val unsolvedCells = this.getUnsolvedCells
    unsolvedCells.forall(cell => this.getValidDigits(cell._1, cell._2).nonEmpty)
  }

  def applyChoice(choice: (Int, Int, Int)): Puzzle = {
    val (row, col, digit) = (choice._1, choice._2, choice._3)
    val updatedIndex = row * 9 + col
    new Puzzle(grid.flatten.updated(updatedIndex, digit).sliding(9, 9).toList)
  }

}