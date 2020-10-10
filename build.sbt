import Dependencies._

ThisBuild / scalaVersion := "2.13.3"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"
ThisBuild / organizationName := "example"

ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision
ThisBuild / scalacOptions += "-Ywarn-unused"

lazy val root = (project in file("."))
  .settings(
    name := "DoomsdayDB",
    libraryDependencies += scalaTest % Test,
    libraryDependencies ++= Seq(
      //"org.scala-lang.modules" %% "scala-collection-contrib" % "0.2.1",
      "org.typelevel" %% "cats-core" % "2.0.0"
    ),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-laws" % "2.0.0" % Test,
      "com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % "1.2.3" % Test,
      "org.scalatest" %% "scalatest" % "3.2.0" % Test,
      "org.typelevel" %% "discipline-scalatest" % "2.0.1" % Test,
      "org.typelevel" %% "alleycats-core" % "2.0.0"
    )
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
