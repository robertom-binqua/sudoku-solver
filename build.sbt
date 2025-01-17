import Dependencies.*

ThisBuild / scalaVersion := "2.13.14"
ThisBuild / version := "0.1.0-SNAPSHOT"

lazy val root = (project in file("."))
  .settings(
    name := "sudoku-solver",
    libraryDependencies += munit % Test,
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.10.0"
  )