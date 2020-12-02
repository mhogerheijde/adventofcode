import Dependencies._


ThisBuild / organization := "net.hogerheijde.aoc"
ThisBuild / version      := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.13.3"
ThisBuild / libraryDependencies ++= Seq(
  fastParse,
  scalaTest % Test,
)

scalacOptions ++= Seq(
  "-encoding", "utf8", // Option and arguments on same line
  "-Xfatal-warnings",  // New lines for each options
  "-deprecation",
  "-unchecked",
  "-language:implicitConversions",
  "-language:higherKinds",
  "-language:existentials",
  "-language:postfixOps"
)

lazy val helpers = project.withId("helpers").in(file("."))
    .settings(
      name := "AoC helpers"
    )
