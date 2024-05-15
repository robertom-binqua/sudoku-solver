package org.binqua.examples.sudoku

import cats.implicits.catsSyntaxEitherId
import munit.FunSuite
import org.binqua.examples.sudoku.ReferenceData.SudoValues._

class GridSpec extends FunSuite {

  test("a valid full grid is recognised valid and solved") {
    val gridAsString =
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

    val actualGrid = Grid.from(string = gridAsString)

    assertEquals(
      (actualGrid.isRight, actualGrid.map(_.isSolved)),
      (true, true.asRight),
      s"grid should be valid but is not. $actualGrid"
    )

  }

  test("a valid incomplete grid is recognised valid and not solved") {
    val gridAsString =
      """| 123 456 789
         | 456 789 123
         | 789 123 -56
         | 234 567 891
         | 567 891 234
         | 891 234 567
         | 345 678 912
         | 678 912 345
         | 912 345 678
         |""".stripMargin

    val actualGrid = Grid.from(string = gridAsString)

    assertEquals(
      (actualGrid.isRight, actualGrid.map(_.isSolved)),
      (true, false.asRight),
      s"grid should be valid but is not. $actualGrid"
    )

  }

  test("a repetition on a line makes a grid invalid. 9 is repeated at line 3") {
    val gridAsString =
      """| 123 456 789
         | 456 789 123
         | 789 123 956
         | 234 567 891
         | 567 891 234
         | 891 234 567
         | 345 678 912
         | 678 912 345
         | 912 345 678
         |""".stripMargin

    val actualGrid = Grid.from(string = gridAsString)

    assertEquals(
      actualGrid,
      "Cannot build square 2. square should not contain duplicated numbers in its lines".asLeft,
      s"grid should be invalid because 9 is repeated. $actualGrid"
    )

  }

  test("newGridWithPositionMovedForward with only 1 - solved it and move current position to none") {
    val gridAsString =
      """| -23 456 789
         | 456 789 123
         | 789 123 456
         | 234 567 891
         | 567 891 234
         | 891 234 567
         | 345 678 912
         | 678 912 345
         | 912 345 678
         |""".stripMargin

    val actualGrid: Either[String, (Boolean, Option[Pos])] = for {
      initialGrid <- Grid.from(gridAsString)
      valueAtCurrentDash <- SudoValue.from(1)
      grid <- initialGrid.buildNewGrid(valueAtCurrentDash)
    } yield (grid.isSolved, grid.currentDashPosition.map(_.pos))

    assertEquals(
      obtained = actualGrid,
      expected = (true, None).asRight
    )

  }

  test("newGridWithPositionMovedForward with only 1 - solved it") {
    val gridAsString =
      """| -23 456 789
         | 456 789 123
         | 789 123 456
         | 234 567 891
         | 567 891 234
         | 891 234 567
         | 345 678 912
         | 678 912 345
         | 912 345 678
         |""".stripMargin

    val actualGrid: Either[String, Boolean] = for {
      initialGrid <- Grid.from(gridAsString)
      grid <- initialGrid.buildNewGrid(one)
    } yield grid.isSolved

    assertEquals(
      obtained = actualGrid,
      expected = true.asRight
    )

  }

  test("newGridWithPositionMovedForward with a grid with only 1 - at the end corner solved it") {
    val gridAsString =
      """| -23 456 789
         | 456 789 123
         | 789 123 456
         | 234 567 891
         | 567 891 234
         | 891 234 567
         | 345 678 912
         | 678 912 345
         | 912 345 67-
         |""".stripMargin

    val actualGrid: Either[String, (Boolean, Option[Pos], Option[Pos])] = for {
      grid0 <- Grid.from(gridAsString)
      grid1 <- grid0.buildNewGrid(newValueAtCurrentDash = one)
      positionAtSquare8 <- grid1.currentDashPosition.map(_.pos).asRight
      finalGrid <- grid1.buildNewGrid(eight)
    } yield (finalGrid.isSolved, positionAtSquare8, finalGrid.currentDashPosition.map(_.pos))

    assertEquals(
      obtained = actualGrid,
      expected = (true, Pos.from(2, 2).toOption, None).asRight
    )

  }

  test("newGridWithPositionMovedForward with only 2 - ....") {
    val gridAsString =
      """| --3 456 789
         | 456 789 123
         | 789 123 456
         | 234 567 891
         | 567 891 234
         | 891 234 567
         | 345 678 912
         | 678 912 345
         | 912 345 678
         |""".stripMargin

    val actualGrid: Either[String, (Boolean, Option[Pos])] = for {
      initialGrid <- Grid.from(gridAsString)
      grid <- initialGrid.buildNewGrid(one)
    } yield (grid.isSolved, grid.currentDashPosition.map(_.pos))

    assertEquals(
      obtained = actualGrid,
      expected = (false, Pos.from(line = 0, column = 1).toOption).asRight
    )

  }

  test("newGridWithPositionMovedForward with only 2 - can be solved in 2 steps") {
    val gridAsString =
      """| --3 456 789
         | 456 789 123
         | 789 123 456
         | 234 567 891
         | 567 891 234
         | 891 234 567
         | 345 678 912
         | 678 912 345
         | 912 345 678
         |""".stripMargin

    val actualGrid: Either[String, (Boolean, Option[Pos])] = for {
      initialGrid <- Grid.from(gridAsString)
      grid <- initialGrid.buildNewGrid(one)
      finalGrid <- grid.buildNewGrid(two)
    } yield (finalGrid.isSolved, finalGrid.currentDashPosition.map(_.pos))

    assertEquals(
      obtained = actualGrid,
      expected = (true, None).asRight
    )

  }

  test("gridWithNewValue moves position from square to square") {
    // 123 456 789
    // 456 789 123
    // 789 ...
    val gridAsString =
      """| 123 -56 789
         | 456 789 123
         | 78- 123 456
         | 234 567 891
         | 567 891 234
         | 891 234 567
         | 345 678 912
         | 678 912 345
         | 912 345 678
         |""".stripMargin

    val actualGrid: Either[String, (Boolean, Option[Pos])] = for {
      grid0 <- Grid.from(gridAsString)
      grid1 <- grid0.buildNewGrid(nine)
      solvedGrid <- grid1.buildNewGrid(four)
    } yield (solvedGrid.isSolved, solvedGrid.currentDashPosition.map(_.pos))

    assertEquals(
      obtained = actualGrid,
      expected = (true, None).asRight
    )

  }

  test("gridWithNewValue moves position in from square to square") {
    // 123 456 789
    // 456 789 123
    // 789 ...
    val gridAsString =
      """| --- -56 789
         | --- 789 123
         | --- 123 456
         | 234 567 891
         | 567 891 234
         | 891 234 567
         | 345 678 912
         | 678 912 345
         | 912 345 678
         |""".stripMargin

    val actualGrid: Either[String, (Boolean, Option[Pos])] = for {
      grid0 <- Grid.from(gridAsString)
      grid1 <- grid0.buildNewGrid(newValueAtCurrentDash = one)
      grid2 <- grid1.buildNewGrid(newValueAtCurrentDash = two)
      grid3 <- grid2.buildNewGrid(newValueAtCurrentDash = three)
      grid4 <- grid3.buildNewGrid(newValueAtCurrentDash = four)
      grid5 <- grid4.buildNewGrid(newValueAtCurrentDash = five)
      grid6 <- grid5.buildNewGrid(newValueAtCurrentDash = six)
      grid7 <- grid6.buildNewGrid(newValueAtCurrentDash = seven)
      grid8 <- grid7.buildNewGrid(newValueAtCurrentDash = eight)
      grid9 <- grid8.buildNewGrid(newValueAtCurrentDash = nine)
      grid10 <- grid9.buildNewGrid(newValueAtCurrentDash = four)
    } yield (grid10.isSolved, grid10.currentDashPosition.map(_.pos))

    assertEquals(
      obtained = actualGrid,
      expected = (true, None).asRight
    )

  }

  test("possibleValuesInCurrentPosition works if it has to find only 1 values in case of vertical -") {
    val gridAsString =
      """| -23 456 789
         | -56 789 123
         | -89 123 456
         | -34 567 891
         | -67 891 234
         | -91 234 567
         | -45 678 912
         | -78 912 345
         | -12 345 67-
         |""".stripMargin

    assertEquals(
      obtained = Grid.from(gridAsString).map(_.possibleValuesInFirstDashPosition()),
      expected = List(one).asRight
    )

  }

  test(
    "possibleValuesInCurrentPosition works if it has to find 1, 2, 4, 7 values in case of vertical and horizontal -"
  ) {
    val gridAsString =
      """| --- --- ---
         | -56 789 123
         | -89 123 456
         | -34 567 891
         | -67 891 234
         | -91 234 567
         | -45 678 912
         | -78 912 345
         | -12 345 67-
         |""".stripMargin

    assertEquals(
      obtained = Grid.from(gridAsString).map(_.possibleValuesInFirstDashPosition()),
      expected = List(one, two, three, four, seven).asRight
    )

  }

  test(
    "possibleValuesInCurrentPosition works if it has to find 1 to 9 values in case of vertical, horizontal and square -"
  ) {
    val gridAsString =
      """| --- --- ---
         | --- 789 123
         | --- 123 456
         | -34 567 891
         | -67 891 234
         | -91 234 567
         | -45 678 912
         | -78 912 345
         | -12 345 67-
         |""".stripMargin

    assertEquals(
      obtained = Grid.from(gridAsString).map(_.possibleValuesInFirstDashPosition()),
      expected = ReferenceData.SudoValues.allValues.asRight
    )

  }

  test(
    "a grid with a column with duplicated values cannot be constructed"
  ) {

    val wrong =
      """| 123 123 123
         | 456 456 456
         | 789 789 789
         | 123 123 123
         | 456 456 456
         | 789 789 789
         | 123 123 123
         | 456 456 456
         | 789 789 789
         |""".stripMargin

    assertEquals(
      obtained = Grid.from(wrong),
      expected =
        "values at column with index 0 (values 1,4,7,1,4,7,1,4,7) contains duplicates. This is not allowed".asLeft
    )
  }

  test(
    "a grid with a line with duplicated values cannot be constructed (line 0)"
  ) {

    val wrong =
      """| 123 789 789
         | 456 456 123
         | 789 123 456
         | 234 567 891
         | 567 891 234
         | 891 234 567
         | 345 678 912
         | 678 912 345
         | 912 345 678
         |""".stripMargin

    assertEquals(
      obtained = Grid.from(wrong),
      expected =
        "values at line with index 0 (values 1,2,3,7,8,9,7,8,9) contains duplicates. This is not allowed".asLeft
    )
  }
}
