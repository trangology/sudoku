import org.scalatest.funsuite.AnyFunSuite

import sudoku.util.*

class ValidPuzzleTest extends AnyFunSuite:
    private lazy val validPuzzle =
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

   test("ValidPuzzle.validate") {
     assert(isValidPuzzle(validPuzzle))
   }

end ValidPuzzleTest