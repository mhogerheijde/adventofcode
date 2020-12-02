import sbt._

object Dependencies {
  val scalatestVersion = "3.2.2"

  val fastParse = "com.lihaoyi" %% "fastparse" % "2.2.2"
  val scalaTest = "org.scalatest" %% "scalatest" % scalatestVersion
  val scalactic = "org.scalactic" %% "scalactic" % scalatestVersion
}
