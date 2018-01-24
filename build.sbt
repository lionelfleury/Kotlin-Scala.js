name := "Kotlin-ScalaJS"
organization := "ch.epfl.k2sjs"

publishMavenStyle := true

version := "0.1-SNAPSHOT"

scalaVersion := "2.12.1"

libraryDependencies ++= Seq(
  "org.jetbrains.kotlin" % "kotlin-compiler-embeddable" % "1.1.61",
  "org.scala-js" %% "scalajs-ir" % "1.0.0-M2",
  "org.scala-js" %% "scalajs-tools" % "1.0.0-M2" % Test,
  "org.scala-js" %% "scalajs-cli" % "1.0.0-M2" % Test,
  "com.github.scopt" %% "scopt" % "3.5.0" % Test,
  "org.scalatest" %% "scalatest" % "3.0.1" % Test
)

parallelExecution in Test := false
