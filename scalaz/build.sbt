import Dependencies._

ThisBuild / scalaVersion     := "2.13.1"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "snorochevskiy"

val scalazVersion = "7.3.0"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % scalazVersion,
  "org.scalaz" %% "scalaz-scalacheck-binding" % scalazVersion % Test
)

scalacOptions in ThisBuild += "-feature"

initialCommands in console := "import scalaz._, Scalaz._"

lazy val root = (project in file("."))
  .settings(
    name := "scalaz-test",
    libraryDependencies += scalaTest % Test
  )
