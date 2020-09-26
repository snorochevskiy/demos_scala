import Dependencies._

ThisBuild / scalaVersion     := "2.13.1"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "snorochevskiy"

libraryDependencies ++= Seq(
  "org.typelevel" %% "simulacrum" % "1.0.0",
  "org.typelevel" %% "cats-core" % "2.0.0",
  "org.typelevel" %% "cats-effect" % "2.2.0"
)

//scalacOptions in ThisBuild += "-feature"
scalacOptions in ThisBuild ++= Seq(
  "-feature",
  "-deprecation",
  "-unchecked",
  "-language:postfixOps",
  "-language:higherKinds",
//  "-Ypartial-unification",
  "-Ymacro-annotations"
)

lazy val root = (project in file("."))
  .settings(
    name := "cats-test",
    libraryDependencies += scalaTest % Test
  )

