import Dependencies._
import Dependencies.fastParse

ThisBuild / credentials += Credentials(Path.userHome / ".sbt" / ".credentials.build")
ThisBuild / resolvers ++= Seq(
  "Nexus @ Hogerheijde" at "https://nexus.hogerheijde.net/repository/hogerheijde/"
)

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "net.hogerheijde.aoc",
      scalaVersion := "2.13.3",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Advent of Code 2015",
    scalastyleConfig := file("../scalastyle-config.xml"),
    libraryDependencies ++= Seq(
      "net.hogerheijde.aoc" %% "aoc-helpers" % "2020.0.0-SNAPSHOT",
      fastParse,
      scalatest % Test,
      scalactic,
    )
  )
