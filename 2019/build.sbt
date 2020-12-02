import Dependencies._

lazy val root = (project in file("."))
  .settings(
    inThisBuild(List(
      organization := "net.hogerheijde.aoc",
      scalaVersion := "2.13.3",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Advent of Code 2019",
    libraryDependencies ++= Seq(
      "net.hogerheijde.aoc" %% "aoc-helpers" % "0.1.0-SNAPSHOT",

      scalatest % Test,
      scalactic,
    )
  )
