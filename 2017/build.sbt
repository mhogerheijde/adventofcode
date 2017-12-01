import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "net.hogerheijde.aoc2017",
      scalaVersion := "2.12.3",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Advent of Code 2017",
    libraryDependencies += scalaTest % Test
  )
