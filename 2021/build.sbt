import Dependencies._

//ThisBuild / resolvers += "Artima Maven Repository" at "https://repo.artima.com/releases"

ThisBuild / organization := "net.hogerheijde.aoc"
ThisBuild / version := "2021-SNAPSHOT"
ThisBuild / scalaVersion := "2.13.3"

lazy val root = (project in file("."))
  .settings(
    name := "Advent of Code 2021",
    scalastyleConfig := file("../scalastyle-config.xml"),

    libraryDependencies ++= Seq(
      "net.hogerheijde.aoc" %% "aoc-helpers" % "2021-SNAPSHOT",
      "com.lihaoyi" %% "fastparse" % "2.3.3",
      scalatest % Test,
      scalactic,
    )
  )
