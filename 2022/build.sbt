import Dependencies._

//ThisBuild / resolvers += "Artima Maven Repository" at "https://repo.artima.com/releases"

ThisBuild / organization := "net.hogerheijde.aoc"
ThisBuild / version := "2022-SNAPSHOT"
ThisBuild / scalaVersion := "3.2.1"

ThisBuild / credentials += Credentials(Path.userHome / ".sbt" / ".credentials.build")
ThisBuild / resolvers ++= Seq(
  "Nexus @ Hogerheijde" at "https://nexus.hogerheijde.net/repository/hogerheijde/"
)


lazy val root = (project in file("."))
  .settings(
    name := "Advent of Code 2022",
    scalastyleConfig := file("../scalastyle-config.xml"),

    libraryDependencies ++= Seq(
      "net.hogerheijde.aoc" %% "aoc-helpers" % "2022.0.3-SNAPSHOT" changing(),
      scalatest % Test,
      scalactic,
    )
  )
