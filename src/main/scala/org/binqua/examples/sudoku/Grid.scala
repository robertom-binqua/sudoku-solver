package org.binqua.examples.sudoku

import cats.implicits.{catsSyntaxEitherId, toBifunctorOps, toTraverseOps}
import org.binqua.examples.sudoku.Grid.neighboursBySquareIndexMapping

object Pos {
  def from(line: Int, column: Int): Either[String, Pos] =
    for {
      lineIndex <- Index.from(line)
      columnIndex <- Index.from(column)
    } yield new Pos(lineIndex, columnIndex) {}
}

object SudoValue {
  def diff(toBeRemoved: List[SudoValue]): List[SudoValue] =
    (1 to 9).toList.diff(toBeRemoved.map(_.value)).map(new SudoValue(_) {})

  def from(value: Int): Either[String, SudoValue] = if (value <= 9 && value >= 1) (new SudoValue(value) {}).asRight
  else s"Sorry only value from 1 to 9. $value is not allowed".asLeft
}

abstract case class SudoValue(value: Int)

object Index {
  def from(value: Int): Either[String, Index] = if (value <= 2 && value >= 0) (new Index(value) {}).asRight
  else s"Sorry only index from 0 to 2. $value is not allowed".asLeft
}

abstract case class Index(value: Int)

abstract case class Pos(lineIndex: Index, columnIndex: Index)

object NineSquaresSorted {
  val validSquareIndexes: Set[Int] = (0 to 8).toSet
  def from(squares: List[Square]): Either[String, NineSquaresSorted] =
    Either.cond(
      squares.map(_.index).toSet == validSquareIndexes,
      right = new NineSquaresSorted(squares.sortBy(_.index)) {},
      left = "squares have to have indexes from 0 to 8"
    )
}

abstract case class NineSquaresSorted(raw: List[Square])

case class Neighbour(mainSquare: Int, vert1: Int, vert2: Int, horiz1: Int, horiz2: Int) {
  val neighboursLineIndexes: List[Int] = List(horiz1, horiz2)
  val neighboursColumnIndexes: List[Int] = List(vert1, vert2)
  val lineIndexes: List[Int] = mainSquare :: neighboursLineIndexes
  val columIndexes: List[Int] = mainSquare :: neighboursColumnIndexes
}

case object Grid {

  val lineIndexes: List[Index] =
    List(0, 1, 2).map(Index.from(_).getOrElse(throw new IllegalStateException("indexes cannot be created")))

  val columnIndexes: List[Index] = lineIndexes

  val neighboursBySquareIndexMapping: Map[Int, Neighbour] = Map(
    0 -> Neighbour(mainSquare = 0, horiz1 = 1, horiz2 = 2, vert1 = 3, vert2 = 6),
    1 -> Neighbour(1, horiz1 = 0, horiz2 = 2, vert1 = 4, vert2 = 7),
    2 -> Neighbour(2, horiz1 = 0, horiz2 = 1, vert1 = 5, vert2 = 8),
    3 -> Neighbour(3, horiz1 = 4, horiz2 = 5, vert1 = 0, vert2 = 6),
    4 -> Neighbour(4, horiz1 = 3, horiz2 = 5, vert1 = 1, vert2 = 7),
    5 -> Neighbour(5, horiz1 = 3, horiz2 = 4, vert1 = 2, vert2 = 8),
    6 -> Neighbour(6, horiz1 = 7, horiz2 = 8, vert1 = 0, vert2 = 3),
    7 -> Neighbour(7, horiz1 = 6, horiz2 = 8, vert1 = 1, vert2 = 4),
    8 -> Neighbour(8, horiz1 = 6, horiz2 = 7, vert1 = 2, vert2 = 5)
  )

  def from(squares: List[Square]): Either[String, Grid] =
    squaresAreValid(squares)

  def from(string: String): Either[String, Grid] = {

    val rawData: Array[String] = string.split("[ \n]").filter(_.nonEmpty)

    val fromSquareIndexToRawDataArrayIndexMapping: Map[Int, Int] = Map(
      0 -> 0,
      1 -> 1,
      2 -> 2,
      3 -> 9,
      4 -> 10,
      5 -> 11,
      6 -> 18,
      7 -> 19,
      8 -> 20
    )

    for {
      squaresBuilt <- squaresFromRawData(fromSquareIndexToRawDataArrayIndexMapping, rawData)
      grid <- squaresAreValid(squares = squaresBuilt)
    } yield grid
  }

  def squaresAreValid(squares: List[Square]): Either[String, Grid] = for {
    nineSquares <- NineSquaresSorted.from(squares)
    _ <- gridColumnsAreValid(nineSquares)
    _ <- gridLinesAreValid(nineSquares)
  } yield new Grid(nineSquares.raw) {}

  private def squaresFromRawData(
      squareIndexNameMapping: Map[Int, Int],
      rawData: Array[String]
  ): Either[String, List[Square]] =
    squareIndexNameMapping
      .map({ case (name, index) => buildSquareAtIndex(index, name, rawData) })
      .toList
      .sequence

  private def buildSquareAtIndex(
      initialIndex: Int,
      squareIndex: Int,
      rawData: Array[String]
  ): Either[String, Square] = (for {
    line1 <- Line.from(0, rawData(initialIndex))
    line2 <- Line.from(1, rawData(initialIndex + 3))
    line3 <- Line.from(2, rawData(initialIndex + 6))
    result <- Square.from(squareIndex, line1, line2, line3)
  } yield result).leftMap(originalError => s"Cannot build square $squareIndex. $originalError")

  private def valuesAreCorrect(sudoValues: List[List[SudoValue]], identifier: String): Either[String, Boolean] = {
    def someValuesAreRepeated(sudoValues: List[SudoValue]): Boolean = {
      val values = sudoValues.map(_.value)
      sudoValues.map(_.value) != values.distinct
    }

    sudoValues.zipWithIndex.find({ case (columnOrLine, _) => someValuesAreRepeated(columnOrLine) }) match {
      case Some((sudoValues, index)) =>
        s"values at $identifier with index $index (values ${sudoValues.map(_.value).mkString(",")}) contains duplicates. This is not allowed".asLeft
      case None => true.asRight
    }
  }

  private def gridColumnsAreValid(nineSquares: NineSquaresSorted): Either[String, Boolean] =
    valuesAreCorrect(gridColumns(nineSquares), "column")

  private def gridLinesAreValid(nineSquares: NineSquaresSorted): Either[String, Boolean] =
    valuesAreCorrect(gridLines(nineSquares), "line")

  private def gridColumns(nineSquares: NineSquaresSorted): List[List[SudoValue]] = {

    def extractSquareColumnValue(squareIndex: Int, columnIndex: Index): List[SudoValue] =
      nineSquares.raw.toArray.apply(squareIndex).valuesAtColumn(columnIndex)

    def extractGridColumnValue(columnIndex: Index, squareInTheColumnIndexes: List[Int]): List[SudoValue] =
      squareInTheColumnIndexes.flatMap(squareIndex => extractSquareColumnValue(squareIndex, columnIndex))

    def extractGridColumnValues(squareIndexesInTheGridColumn: List[Int]): List[List[SudoValue]] =
      Grid.columnIndexes.map(columnIndex => extractGridColumnValue(columnIndex, squareIndexesInTheGridColumn))

    val squareIndexesAtTheTopOfTheGrid = List(0, 1, 2)

    squareIndexesAtTheTopOfTheGrid.flatMap(index =>
      extractGridColumnValues(neighboursBySquareIndexMapping(index).columIndexes)
    )
  }

  private def gridLines(squares: NineSquaresSorted): List[List[SudoValue]] = {
    def lineValues(squareIndex: Int, lineIndex: Index): List[SudoValue] =
      squares.raw.toArray.apply(squareIndex).valuesAtLine(lineIndex)

    def fullLineValues(squareIndexesInTheLine: List[Int], lineIndex: Index): List[SudoValue] =
      squareIndexesInTheLine.flatMap(squareIndex => lineValues(squareIndex, lineIndex))

    def extractLines(squareIndexes: List[Int]): List[List[SudoValue]] =
      Grid.lineIndexes.map(lineIndex => fullLineValues(squareIndexes, lineIndex))

    def squaresOnTheSameLineOfSquare(squareIndex: Int): List[Int] =
      neighboursBySquareIndexMapping(squareIndex).lineIndexes

    val squareIndexesAtTheLeftOfTheGrid = List(0, 3, 6)

    squareIndexesAtTheLeftOfTheGrid.flatMap(squareIndex => extractLines(squaresOnTheSameLineOfSquare(squareIndex)))
  }
}

case class DashPosition(pos: Pos, square: Square)

abstract case class Grid(squares: List[Square]) {

  val currentDashPosition: Option[DashPosition] = squares
    .find(_.firstDashPosition.isDefined)
    .flatMap(_.firstDashPosition)

  val isSolved: Boolean = squares.forall(_.isSolved)

  def possibleValuesInFirstDashPosition(): List[SudoValue] = currentDashPosition
    .map(currentPosition =>
      removeNeighboursValues(
        allowedValues = currentPosition.square.possibleValuesAtFirstDash.get,
        squareNeighboursIndexMapping = neighboursBySquareIndexMapping,
        dashPosition = currentPosition,
        squares = squares
      )
    )
    .getOrElse(List.empty)

  private def removeNeighboursValues(
      allowedValues: List[SudoValue],
      squareNeighboursIndexMapping: Map[Int, Neighbour],
      dashPosition: DashPosition,
      squares: List[Square]
  ): List[SudoValue] = {
    val neighbour: Neighbour = squareNeighboursIndexMapping(dashPosition.square.index)
    val lineValuesInTheNeighbours = neighbour.neighboursLineIndexes
      .map(squares)
      .flatMap(_.valuesAtLine(dashPosition.pos.lineIndex))
    val columnValuesInTheNeighbours = neighbour.neighboursColumnIndexes
      .map(squares)
      .flatMap(_.valuesAtColumn(dashPosition.pos.columnIndex))

    allowedValues.diff(lineValuesInTheNeighbours ::: columnValuesInTheNeighbours)
  }

  def buildNewGrid(newValueAtCurrentDash: SudoValue): Either[String, Grid] = for {
    dashPosition <- currentDashPosition.toRight("Grid is already completed")
    newSquare <- dashPosition.square.newSquare(dashPosition.pos, newValueAtCurrentDash)
    newGrid <- Grid.from(squares.updated(newSquare.index, newSquare))
  } yield newGrid

}
