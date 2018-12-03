import Dependencies._


ThisBuild / organization := "net.hogerheijde.aoc"
ThisBuild / version      := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.12.7"
ThisBuild / libraryDependencies ++= Seq(
  fastParse,
  scalaTest % Test,
)

lazy val helpers = project

lazy val aoc2017 = project
  .in(file("2017"))
  .dependsOn(helpers)
  .settings(
    name := "2017"
  )

lazy val aoc2018 = project
  .in(file("2018"))
  .dependsOn(helpers)
  .settings(
    name := "2018"
  )
