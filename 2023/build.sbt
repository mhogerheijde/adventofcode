import Dependencies.*

ThisBuild / organization := "net.hogerheijde.aoc"
ThisBuild / version := "2023-SNAPSHOT"
ThisBuild / scalaVersion := "3.2.1"

ThisBuild / credentials += Credentials(Path.userHome / ".sbt" / ".credentials.build")
ThisBuild / resolvers ++= Seq(
  "Nexus @ Hogerheijde" at "https://nexus.hogerheijde.net/repository/hogerheijde/"
)

ThisBuild / scalacOptions ++= Seq( // use ++= to add to existing options
  "-encoding", "utf8", // if an option takes an arg, supply it on the same line
  "-unchecked",
  "-Wunused:all",
)

lazy val root = (project in file("."))
  .settings(
    name := "Advent of Code 2023",
    scalastyleConfig := file("../scalastyle-config.xml"),

    libraryDependencies ++= Seq(
      "net.hogerheijde.aoc" %% "aoc-helpers" % "2023.0.1-SNAPSHOT" changing(),
      "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
      "com.lihaoyi" %% "fastparse" % "3.0.2",
      scalatest % Test,
    )
  )
