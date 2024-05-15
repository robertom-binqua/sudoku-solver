package org.binqua.examples.sudoku

object ReferenceData {

  object SudoValues {
    val one: SudoValue = SudoValue.from(1).getOrElse(throw new IllegalArgumentException("this is embarassing"))
    val two: SudoValue = SudoValue.from(2).getOrElse(throw new IllegalArgumentException("this is embarassing"))
    val three: SudoValue = SudoValue.from(3).getOrElse(throw new IllegalArgumentException("this is embarassing"))
    val four: SudoValue = SudoValue.from(4).getOrElse(throw new IllegalArgumentException("this is embarassing"))
    val five: SudoValue = SudoValue.from(5).getOrElse(throw new IllegalArgumentException("this is embarassing"))
    val six: SudoValue = SudoValue.from(6).getOrElse(throw new IllegalArgumentException("this is embarassing"))
    val seven: SudoValue = SudoValue.from(7).getOrElse(throw new IllegalArgumentException("this is embarassing"))
    val eight: SudoValue = SudoValue.from(8).getOrElse(throw new IllegalArgumentException("this is embarassing"))
    val nine: SudoValue = SudoValue.from(9).getOrElse(throw new IllegalArgumentException("this is embarassing"))
    val allValues: List[SudoValue] = (1 to 9).toList.map(SudoValue.from(_).toOption.get)

  }

}
