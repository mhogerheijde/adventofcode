import Dependencies.*
import com.typesafe.tools.mima.core.ProblemFilters
import com.typesafe.tools.mima.core.ReversedMissingMethodProblem

ThisBuild / organization  := "net.hogerheijde.aoc"
ThisBuild / version       := "2023.0.1-SNAPSHOT"
ThisBuild / scalaVersion  := "3.2.1"
ThisBuild / versionScheme := Some("semver-spec")
ThisBuild / libraryDependencies ++= Seq(
  "com.lihaoyi" %% "fastparse" % "3.0.2",
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
  "Snapshots @ Hogerheijde" at "https://nexus.hogerheijde.net/repository/hogerheijde-snapshots/",
  "Releases @ Hogerheijde" at "https://nexus.hogerheijde.net/repository/hogerheijde-releases/",
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
      mimaPreviousArtifacts := Set (
        "net.hogerheijde.aoc" %% "aoc-helpers" % "2023.0.0",
      ),
    )


ThisBuild / mimaBinaryIssueFilters ++= {
  // These things are Ops classes that shouldn't have the `value` exposed. These should have never been public because they don't
  // provide any value. Making them private because of issues like #2514 and #2613.
  Seq(
    ProblemFilters.exclude[ReversedMissingMethodProblem]("net.hogerheijde.aoc.util.CircularBuffer.headOption"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("net.hogerheijde.aoc.util.CircularBuffer.rotate"),
  )
}