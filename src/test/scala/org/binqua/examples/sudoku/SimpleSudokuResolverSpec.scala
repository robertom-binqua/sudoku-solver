package org.binqua.examples.sudoku

import cats.implicits.catsSyntaxEitherId
import munit.FunSuite

class SimpleSudokuResolverSpec extends FunSuite {

  test("resolve can do its job ;-)") {

    val solvedAsString =
      """| 123 456 789
         | 456 789 123
         | 789 123 456
         | 234 567 891
         | 567 891 234
         | 891 234 567
         | 345 678 912
         | 678 912 345
         | 912 345 678
         |""".stripMargin

    val gridAsString =
      """| -23 -56 7--
         | -56 -89 12-
         | -89 -2- 45-
         | -34 -67 89-
         | -67 -91 23-
         | 891 -34 56-
         | -45 -78 ---
         | -78 -12 ---
         | -12 -45 ---
         |""".stripMargin

    assertEquals(
      obtained = Grid.from(gridAsString).map(SimpleSudokuResolver.resolve),
      expected = Grid.from(solvedAsString)
    )
  }

}
