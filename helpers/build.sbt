import Dependencies.*
import com.typesafe.tools.mima.core.ProblemFilters
import com.typesafe.tools.mima.core.ReversedMissingMethodProblem

ThisBuild / organization  := "net.hogerheijde.aoc"
ThisBuild / version       := "2024.0.0-SNAPSHOT"
ThisBuild / scalaVersion  := "3.2.2"
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

initialize := {
  // Ensure previous initializations are run
  val _ = initialize.value

  // Retrieve the JVM's class version and specification version
  val classVersion = sys.props("java.class.version")
  val specVersion = sys.props("java.specification.version")

  // Assert that the JVM meets the minimum required version, for example, Java 17
  assert(specVersion.toDouble == 17, "Java 17 is required to build this project.")
}

ThisBuild / resolvers ++= Seq(
//  "Snapshots @ Hogerheijde" at "https://nexus.hogerheijde.net/repository/hogerheijde-snapshots/",
//  "Releases @ Hogerheijde" at "https://nexus.hogerheijde.net/repository/hogerheijde-releases/",
  "Mirror @ Hogerheijde" at "https://nexus.hogerheijde.net/repository/mirror/",
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
      libraryDependencies += "org.rogach" %% "scallop" % "5.1.0",
      mimaPreviousArtifacts := Set (
        "net.hogerheijde.aoc" %% "aoc-helpers" % "2023.0.0",
        "net.hogerheijde.aoc" %% "aoc-helpers" % "2024.0.0",
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
