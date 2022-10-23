import Dependencies._


ThisBuild / organization  := "net.hogerheijde.aoc"
ThisBuild / version       := "2020.0.0-SNAPSHOT"
ThisBuild / scalaVersion  := "2.13.3"
ThisBuild / versionScheme := Some("semver-spec")
ThisBuild / libraryDependencies ++= Seq(
  fastParse,
  scalaTest % Test,
)

ThisBuild / scalacOptions ++= Seq(
  "-encoding", "utf8", // Option and arguments on same line
  "-Xfatal-warnings",  // New lines for each options
  "-deprecation",
  "-unchecked",
  "-language:implicitConversions",
  "-language:higherKinds",
  "-language:existentials",
  "-language:postfixOps"
)

ThisBuild / resolvers ++= Seq(
  "Nexus @ Hogerheijde" at "https://nexus.hogerheijde.net/repository/hogerheijde/"
)
ThisBuild / credentials += Credentials(Path.userHome / ".sbt" / ".credentials.deploy")
ThisBuild / publishMavenStyle := true
ThisBuild / publishTo := {
  if (isSnapshot.value)
    Some("snapshots" at "https://nexus.hogerheijde.net/repository/hogerheijde-snapshots/")
  else
    Some("releases" at "https://nexus.hogerheijde.net/repository/hogerheijde-releases/")
}

lazy val helpers = project.withId("helpers").in(file("."))
    .settings(
      name := "AoC helpers",
      mimaPreviousArtifacts := Set ("net.hogerheijde.aoc" %% "aoc-helpers" % "2020"),
    )
