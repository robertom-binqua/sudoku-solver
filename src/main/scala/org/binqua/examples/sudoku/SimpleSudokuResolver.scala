package org.binqua.examples.sudoku

import cats.implicits.catsSyntaxEitherId

import scala.annotation.tailrec

object SimpleSudokuResolver extends SudokuResolver {
  private val noMoreValuesForCurrentDashPositionError: Left[String, Grid] = Left(
    "not solution possible: there are not more values to try"
  )

  override def resolve(aGrid: Grid): Grid =
    internalResolve(aGrid) match {
      case Left(error)       => throw new IllegalStateException(s"this is embarrassing. $error")
      case Right(gridSolved) => gridSolved
    }

  private def internalResolve(aGrid: Grid): Either[String, Grid] =
    if (aGrid.isSolved) aGrid.asRight
    else
      tryToSolveByChangingValue(aGrid, valuesToTry = aGrid.possibleValuesInFirstDashPosition())

  @tailrec
  private def tryToSolveByChangingValue(aGrid: Grid, valuesToTry: List[SudoValue]): Either[String, Grid] =
    valuesToTry match {
      case ::(value, remainingValues) =>
        aGrid.buildNewGrid(newValueAtCurrentDash = value) match {
          case Right(aNewGrid) =>
            internalResolve(aNewGrid) match {
              case solution @ Right(_)                     => solution
              case `noMoreValuesForCurrentDashPositionError` => tryToSolveByChangingValue(aGrid, remainingValues)
              case error                                   => error
            }
          case error => error
        }
      case Nil =>
        noMoreValuesForCurrentDashPositionError
    }

}
