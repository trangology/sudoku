package sudoku.utils

class Helper {
  private def isValid(region: List[Int]): Boolean = {
    val nonZeroRegion = region.filterNot(_ == 0)
    if (nonZeroRegion.distinct.length != nonZeroRegion.length) false
    else nonZeroRegion.forall((1 to 9).contains)
  }

  private def validateRows(grid: List[List[Int]]): Boolean = {
    grid.forall(row => isValid(row))
  }

  private def validateColumns(grid: List[List[Int]]): Boolean = {
    (0 to 8).forall(i => isValid(grid.map(_ (i))))
  }

  private def validateBlocks(grid: List[List[Int]]): Boolean = {
    val slideList = grid.flatten.sliding(27, 27).toList
    slideList.forall(slide => {
      val blocks = slide.sliding(3, 3).toList
      (0 to 2).forall(i => isValid(blocks(i) ::: blocks(i + 3) ::: blocks(i + 6)))
    })
  }

  private def drawRow(grid: List[List[Int]], row: Int, separator: String): String = {
    val newRow = grid(row).sliding(3, 3).toList
    val stringRow = for (slide <- newRow) yield (slide.mkString(" ") + separator)
    val result = separator + stringRow(0) + stringRow(1) + stringRow(2)
    result.replace("0", " ").stripTrailing() + "\n"
  }

  def isValidSudoku(rawSudoku: List[List[Int]]): Boolean = {
    validateRows(rawSudoku) && validateColumns(rawSudoku) && validateBlocks(rawSudoku)
  }

  def renderSudoku(grid: List[List[Int]]): String = {
    val ROW_INDICES = "  | 1 2 3 | 4 5 6 | 7 8 9 |"
    val ROW_DECORATOR = "--+-------+-------+-------+"
    val COL_DECORATOR = " | "

    val rows = (0 to 8).map(row => row.toString + drawRow(grid, row, COL_DECORATOR)).toList
    val slides = (0 to 8).by(3).map(i => rows(i) + rows(i + 1) + rows(i + 2) + ROW_DECORATOR).mkString("\n")
    s"\n${ROW_INDICES}\n${ROW_DECORATOR}\n$slides\n"
  }
}
