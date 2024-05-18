# README #

### What is this repository for? ###

* This is a simple solution in scala for the famous [Sudoku](https://en.wikipedia.org/wiki/Sudoku)

### How do I get set up? ###

The project uses sbt. Open a sbt shell and use test task to run all the tests:

```bash
     sbt
```


```bash
sbt:sudoku-solver> test
org.binqua.examples.sudoku.SquareSpec:
  + lines should not contain duplicate!! 1 is duplicated here 0.153s
  + lines with no duplicate are valid 0.001s
  + possibleValuesAt ... 4 is the only possible value 0.002s
  + possibleValuesAt ... 4 and 5 are the only possible value 0.001s
  + possibleValuesAt ... all the values are the possible 0.0s
org.binqua.examples.sudoku.LineTest:
  + Line 123 is correct 0.153s
  + Line --- is correct 0.001s
  + Line 991 is not correct 0.0s
  + Line with 0 is not correct 0.0s
  + newLine can build a new line from --- and position Pos.from(line = 0, column = 0, square = 1) 0.003s
  + newLine can build a new line from 1-- and position Pos.from(line = 0, column = 1, square = 1) 0.0s
  + newLine can build a new line from 12- and position Pos.from(line = 0, column = 2, square = 1) 0.001s
  + newLine cannot build a new line if column has already a value) 0.0s
org.binqua.examples.sudoku.SimpleSudokuResolverSpec:
  + resolve can do its job ;-) 0.225s
org.binqua.examples.sudoku.GridSpec:
  + a valid full grid is recognised valid and solved 0.202s
  + a valid incomplete grid is recognised valid and not solved 0.002s
  + a repetition on a line makes a grid invalid. 9 is repeated at line 3 0.001s
  + buildNewGrid with only 1 - solved it and move current position to none 0.002s
  + buildNewGrid with only 1 - solved it 0.003s
  + buildNewGrid with a grid with only 1 - at the end corner solved it 0.003s
  + buildNewGrid with only 2 - .... 0.003s
  + buildNewGrid with only 2 - can be solved in 2 steps 0.003s
  + gridWithNewValue moves position from square to square 0.002s
  + gridWithNewValue moves position in from square to square 0.005s
  + possibleValuesInFirstDashPosition works if it has to find only 1 values in case of vertical - 0.001s
  + possibleValuesInFirstDashPosition works if it has to find 1, 2, 4, 7 values in case of vertical and horizontal - 0.001s
  + possibleValuesInFirstDashPosition works if it has to find 1 to 9 values in case of vertical, horizontal and square - 0.001s
  + a grid with a column with duplicated values cannot be constructed 0.0s
  + a grid with a line with duplicated values cannot be constructed (line 0) 0.0s
org.binqua.examples.sudoku.GridPrettyPrintSpec:
  + prettyPrint it works great 0.203s
  + prettyPrint can do its job in case almost an empty initial grid 0.085s
[info] Passed: Total 31, Failed 0, Errors 0, Passed 31
[success] Total time: 1 s, completed 18 May 2024, 15:25:01
```
