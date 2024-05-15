package org.binqua.examples.sudoku

import cats.implicits.{catsSyntaxEitherId, catsSyntaxOptionId}

import scala.util.Try

object Square {

  private def fromLines(name: Int, lines: Array[Line]): Either[String, Square] =
    lines.toList match {
      case f :: s :: t :: Nil => from(name, f, s, t)
      case _                  => "You must have exactly 3 lines".asLeft
    }

  def from(name: Int, line1: Line, line2: Line, line3: Line): Either[String, Square] =
    if (haveSomeDuplicates(line1.numbers ::: line2.numbers ::: line3.numbers))
      Left("square should not contain duplicated numbers in its lines")
    else Right(new Square(name, line1, line2, line3) {})

  private def haveSomeDuplicates(allNumbers: List[SudoValue]): Boolean =
    allNumbers.length != allNumbers.toSet.size
}

abstract case class Square(index: Int, line1: Line, line2: Line, line3: Line) {

  private val lines: Array[Line] = Array(line1, line2, line3)

  val isSolved: Boolean = line1.isSolved && line2.isSolved && line3.isSolved

  def firstDashPosition: Option[DashPosition] = lines
    .find(_.dashCoordinates.isDefined)
    .flatMap(_.dashCoordinates)
    .flatMap { case (r, c) => Pos.from(r.value, c.value).toOption.map(DashPosition(_, this)) }

  def newSquare(position: Pos, newValue: SudoValue): Either[String, Square] = for {
    lineUpdated <- Line.newLine(oldLine = lines(position.lineIndex.value), position.columnIndex, newValue)
    newSquare <- Square.fromLines(this.index, lines.updated(position.lineIndex.value, lineUpdated))
  } yield newSquare

  def possibleValuesAtFirstDash: Option[List[SudoValue]] =
    firstDashPosition
      .map(_.pos)
      .flatMap(pos =>
        if (lines(pos.lineIndex.value).values(pos.columnIndex.value).nonEmpty) None
        else
          SudoValue.diff(lines.toList.flatMap(_.numbers)).some
      )


  def valuesAtLine(lineIndex: Index): List[SudoValue] =
    lines(lineIndex.value).numbers

  def valuesAtColumn(columnIndex: Index): List[SudoValue] =
    lines.map(_.valueAtColumn(columnIndex)).filter(_.isDefined).map(_.get).toList

}

object Line {
  private def fromSudoValues(lineIndex: Index, values: List[Option[SudoValue]]): Either[String, Line] =
    Either.cond(noDuplication(values), new Line(lineIndex, values) {}, "No duplication please")

  private def noDuplication(values: List[Option[SudoValue]]): Boolean = {
    val allValues = values
      .partitionMap({
        case None        => "".asLeft
        case Some(value) => value.asRight
      })
      ._2
      .map(_.value)
    allValues.size == allValues.distinct.size
  }

  def from(lineIndex: Int, data: String): Either[String, Line] = for {
    numbersOrDashes <- has3NumbersOrDashes(data)
    _ <- doesNotHaveDuplicate(data)
    index <- Index.from(lineIndex)
  } yield new Line(index, numbersOrDashes) {}

  private def doesNotHaveDuplicate(data: String): Either[String, Unit] =
    if (data.toList.toSet.size == data.toList.size || has2Or3Dashes(data)) Right(())
    else Left("duplicated numbers")

  private def has2Or3Dashes(data: String): Boolean = {
    val dashes = data.count(_ == '-')
    dashes == 2 || dashes == 3
  }

  private def has3NumbersOrDashes(
      data: String
  ): Either[String, List[Option[SudoValue]]] = if (data.toList.size == 3)
    data.toList.map(toSudoValue) match {
      case Right(f) :: Right(s) :: Right(t) :: Nil => Right(List(f, s, t))
      case _                                       => Left("line has to be formed by numbers from 1..9 or dashes")
    }
  else
    Left(s"line has to be formed by 3 numbers or dashes. $data is not correct")

  private def toSudoValue(c: Char): Either[String, Option[SudoValue]] = c match {
    case '-' => Right(None)
    case '0' => Left("zero is not valid")
    case c   => Try(c.asDigit).toEither.left.map(_.toString).flatMap(SudoValue.from).map(_.some)
  }

  def newLine(oldLine: Line, columnIndex: Index, value: SudoValue): Either[String, Line] = {
    def valueIsEmpty(oldLine: Line, columnIndex: Index): Either[String, Unit] =
      Either.cond(
        oldLine.valueAtColumn(columnIndex).isEmpty,
        (),
        s"At column $columnIndex there is a value. It should be empty"
      )

    for {
      _ <- valueIsEmpty(oldLine, columnIndex)
      newLine <- Line.fromSudoValues(
        lineIndex = oldLine.lineIndex,
        values = oldLine.values.updated(columnIndex.value, Some(value))
      )
    } yield newLine
  }

}

sealed abstract case class Line(lineIndex: Index, values: List[Option[SudoValue]]) {

  val numbers: List[SudoValue] = values.partitionMap {
    case Some(validNumber) => Right(validNumber)
    case None              => Left("")
  }._2

  val isSolved: Boolean = values.forall(_.isDefined)

  val dashCoordinates: Option[(Index, Index)] =
    if (values.indexOf(None) != -1)
      Index.from(values.indexOf(None)).toOption.map(columnIndex => (lineIndex, columnIndex))
    else None

  def valueAtColumn(columnIndex: Index): Option[SudoValue] = values.drop(columnIndex.value).headOption.flatten

}
