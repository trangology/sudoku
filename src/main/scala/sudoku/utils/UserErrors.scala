package sudoku.utils

sealed trait UserErrors {
  protected def errorMessage: String
}


case class IllegalDigit(digit: Int) extends UserErrors {
  override val errorMessage: String =
    """ERROR: Your digit is %d.
      |But legal digit must be any integer started from 1 and 9.
      |""".stripMargin.format(digit)
}


case class InvalidDigit(userDigit: Int, validDigits: List[Int]) extends UserErrors {
  override val errorMessage: String =
    """ERROR: %d existed.
      |Want hint? Here is the list of valid digits:
      |%s
      |""".stripMargin.format(userDigit, validDigits.mkString(", "))
}


case class NotUnsolvedCell(row: Int, col: Int, unsolvedCells: List[(Int, Int)]) extends UserErrors {
  override val errorMessage: String =
    """ERROR: (%d, %d) is not empty cell.
      |Want hint? Below is the list of valid cells:
      |%s.
      |""".stripMargin.format(row, col + 1, (unsolvedCells.map(cell => (cell._1, cell._2 + 1))).mkString(", "))
}


case class IllegalRow(row: Int, validRows: List[Int]) extends UserErrors {
  override val errorMessage: String = {
    """ERROR: Row %d is out of grid.
      |Want hint? Below is the list of valid rows:
      |%s
      |""".stripMargin.format(row, validRows.sorted.mkString(", "))
  }
}


case class IllegalCol(row: Int, col: Int, validColumns: List[Int]) extends UserErrors {
  override val errorMessage: String =
    """Column %d is out of grid.
      |Want hint? Below is the list of valid columns with row %d:
      |%s
      |""".format(col + 1, row, validColumns.map(col => col + 1).sorted.mkString(", ")).stripMargin
}


case class IllegalCell(row: Int, col: Int, unsolvedCells: List[(Int, Int)]) extends UserErrors {
  override val errorMessage: String =
    """ERROR: Cell (%d, %d) is out of grid.
      |Please choose another cell from this list:
      |%s
      |""".stripMargin.format(row, col, (unsolvedCells.map(cell => (cell._1, cell._2 + 1))).mkString(", "))
}


case class UnknownCharacter() extends UserErrors {
  override val errorMessage: String =
    """ERROR: Your input contains special character/characters.
      |Values of columns must be integers started from 0 to 8.
      |Values of rows and digits must be integers started from 1 to 9.
      |""".stripMargin.format()
}
