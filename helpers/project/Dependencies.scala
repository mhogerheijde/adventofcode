import sbt._

object Dependencies {
  val scalatestVersion = "3.2.10"

  val fastParse = "com.lihaoyi" %% "fastparse" % "2.3.3"
  val scalaTest = "org.scalatest" %% "scalatest" % scalatestVersion
  val scalactic = "org.scalactic" %% "scalactic" % scalatestVersion
}
