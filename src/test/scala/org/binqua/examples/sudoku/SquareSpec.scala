package org.binqua.examples.sudoku

import cats.implicits.catsSyntaxEitherId
import munit.FunSuite
import org.binqua.examples.sudoku.ReferenceData.SudoValues.{five, four}

class SquareSpec extends FunSuite {

  test("lines should not contain duplicate!! 1 is duplicated here") {
    val result: Either[String, Square] = for {
      line1 <- Line.from(0, "123")
      line2 <- Line.from(0, "178")
      line3 <- Line.from(0, "456")
      result <- Square.from(0, line1, line2, line3)
    } yield result

    assertEquals(
      result.isLeft,
      true,
      "square should not contain duplicated numbers in its lines"
    )
  }

  test("lines with no duplicate are valid") {
    val result: Either[String, Square] = for {
      line1 <- Line.from(0, "123")
      line2 <- Line.from(0, "456")
      line3 <- Line.from(0, "789")
      result <- Square.from(0, line1, line2, line3)
    } yield result

    assertEquals(
      result.isRight,
      true,
      "lines with no duplicate should be valid"
    )
  }

  test("possibleValuesAt ... 4 is the only possible value") {
    val result: Either[String, Option[List[SudoValue]]] = for {
      line1 <- Line.from(0, "123")
      line2 <- Line.from(1, "-56")
      line3 <- Line.from(2, "789")
      square <- Square.from(0, line1, line2, line3)
      pos <- Pos.from(1, 0)
    } yield square.possibleValuesAtFirstDash

    assertEquals(
      result,
      Some(List(four)).asRight
    )
  }

  test("possibleValuesAt ... 4 and 5 are the only possible value") {
    val result: Either[String, Option[List[SudoValue]]] = for {
      line1 <- Line.from(0, "123")
      line2 <- Line.from(1, "--6")
      line3 <- Line.from(2, "789")
      square <- Square.from(0, line1, line2, line3)
    } yield square.possibleValuesAtFirstDash

    assertEquals(
      result,
      Some(List(four, five)).asRight
    )
  }

  test("possibleValuesAt ... all the values are the possible") {
    val result: Either[String, Option[List[SudoValue]]] = for {
      line1 <- Line.from(0, "---")
      line2 <- Line.from(0, "---")
      line3 <- Line.from(0, "---")
      square <- Square.from(0, line1, line2, line3)
    } yield square.possibleValuesAtFirstDash

    assertEquals(
      result,
      expected = Some(ReferenceData.SudoValues.allValues).asRight
    )
  }

}
