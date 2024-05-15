package org.binqua.examples.sudoku

import cats.implicits.catsSyntaxEitherId
import munit.FunSuite

class GridPrettyPrintSpec extends FunSuite {

  test("prettyPrint it works great") {

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

    val expected =
      """|123 | 456 | 789
         |456 | 789 | 123
         |789 | 123 | 456
         |---------------
         |234 | 567 | 891
         |567 | 891 | 234
         |891 | 234 | 567
         |---------------
         |345 | 678 | 912
         |678 | 912 | 345
         |912 | 345 | 678
         |""".stripMargin

    assertEquals(
      obtained = Grid.from(solvedAsString).map(_.prettyPrint),
      expected = expected.asRight
    )
  }

  test("prettyPrint can do its job in case almost an empty initial grid") {

    val gridAsString =
      """| 4-- --- 9--
         | --- --- ---
         | --- --- ---
         | --- --- ---
         | --- --- ---
         | --- 123 ---
         | --- --- ---
         | -5- --- ---
         | --- --- --8
         |""".stripMargin

    val solvedAsString =
      """|412 | 356 | 987
         |365 | 789 | 124
         |789 | 214 | 356
         |---------------
         |123 | 465 | 879
         |546 | 897 | 213
         |897 | 123 | 465
         |---------------
         |234 | 578 | 691
         |658 | 931 | 742
         |971 | 642 | 538
         |""".stripMargin

    assertEquals(
      obtained = Grid.from(gridAsString).map(SimpleSudokuResolver.resolve).map(_.prettyPrint),
      expected = solvedAsString.asRight
    )
  }

}
