package org.binqua.examples.sudoku

import munit.FunSuite
import org.binqua.examples.sudoku.ReferenceData.SudoValues.{one, three, two}

class LineTest extends FunSuite {

  test("Line 123 is correct") {
    assertEquals(Line.from(0, "123").isRight, true)
  }

  test("Line --- is correct") {
    assertEquals(Line.from(0, "---").isRight, true)
  }

  test("Line 991 is not correct") {
    assertEquals(Line.from(0, "991"), Left("duplicated numbers"))
  }

  test("Line with 0 is not correct") {
    assertEquals(Line.from(0, "102"), Left("line has to be formed by numbers from 1..9 or dashes"))
  }

  test("newLine can build a new line from --- and position Pos.from(line = 0, column = 0, square = 1)") {
    val actualUpdateLine: Either[String, Line] = for {
      oldLine <- Line.from(lineIndex = 0, data = "---")
      column <- Index.from(0)
      updatedLine <- Line.newLine(oldLine, column, one)
    } yield updatedLine

    assertEquals(actualUpdateLine, Line.from(0, "1--"))
  }

  test("newLine can build a new line from 1-- and position Pos.from(line = 0, column = 1, square = 1)") {
    val actualUpdateLine = for {
      oldLine <- Line.from(lineIndex = 0, data = "1--")
      column <- Index.from(1)
      updatedLine <- Line.newLine(oldLine, column, two)
    } yield updatedLine

    assertEquals(actualUpdateLine.isRight, true, s"$actualUpdateLine should be valid but is not ")

    assertEquals(actualUpdateLine, Line.from(0, "12-"))
  }

  test("newLine can build a new line from 12- and position Pos.from(line = 0, column = 2, square = 1)") {
    val actualUpdateLine = for {
      oldLine <- Line.from(lineIndex = 1, data = "12-")
      column <- Index.from(2)
      updatedLine <- Line.newLine(oldLine, column, three)
    } yield updatedLine

    assertEquals(actualUpdateLine.isRight, true, s"$actualUpdateLine should be valid but is not ")

    assertEquals(actualUpdateLine, Line.from(1, "123"))
  }

  test("newLine cannot build a new line if column has already a value)") {
    val actualUpdateLine = for {
      oldLine <- Line.from(lineIndex = 1, data = "123")
      column <- Index.from(2)
      updatedLine <- Line.newLine(oldLine, column, three)
    } yield updatedLine

    assertEquals(actualUpdateLine.isLeft, true, s"$actualUpdateLine should be invalid but it looks valid")

    assertEquals(
      actualUpdateLine,
      Left("At column Index(2) there is a value. It should be empty")
    )
  }

}
