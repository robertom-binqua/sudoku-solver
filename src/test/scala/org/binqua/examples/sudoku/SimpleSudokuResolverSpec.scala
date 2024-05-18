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

  test("we can solve sudoku from https://en.wikipedia.org/wiki/Sudoku") {

    val solvedAsString =
      """|534 678 912
         |672 195 348
         |198 342 567
         |859 761 423
         |426 853 791
         |713 924 856
         |961 537 284
         |287 419 635
         |345 286 179""".stripMargin

    val gridAsString =
      """| 53- -7- ---
         | 6-- 195 ---
         | -98 --- -6-
         | 8-- -6- --3
         | 4-- 8-3 --1
         | 7-- -2- --6
         | -6- --- 28-
         | --- 419 --5
         | --- -8- -79
         |""".stripMargin

    assertEquals(
      obtained = Grid.from(gridAsString).map(SimpleSudokuResolver.resolve),
      expected = Grid.from(solvedAsString)
    )
  }

}
