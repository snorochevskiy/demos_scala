import Dependencies._

ThisBuild / scalaVersion     := "2.13.3"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "snorochevskiy"
ThisBuild / organizationName := "Stas Norochevskiy"

lazy val root = (project in file("."))
  .settings(
    name := "fp-basics",
    libraryDependencies += scalaTest % Test
  )
