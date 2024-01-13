import Dependencies._

ThisBuild / organization  := "net.hogerheijde.aoc"
ThisBuild / version       := "2023.0.0"
ThisBuild / scalaVersion  := "3.2.1"
ThisBuild / versionScheme := Some("semver-spec")
ThisBuild / libraryDependencies ++= Seq(
  scalaTest % Test,
)

ThisBuild / scalacOptions ++= Seq(
  "-encoding", "utf8",
  "-Xfatal-warnings",
  "-explain",
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
//ThisBuild / overridePublishSettings := true


lazy val helpers = project.withId("helpers").in(file("."))
    .settings(
      name := "AoC helpers",
      mimaPreviousArtifacts := Set ("net.hogerheijde.aoc" %% "aoc-helpers" % "2022.0.0-SNAPSHOT"),
    )
