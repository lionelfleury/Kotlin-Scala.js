name := "Kotlin-Scalajs"

version := "0.1-SNAPSHOT"

scalaVersion := "2.12.1"

libraryDependencies ++= Seq(
  "org.jetbrains.kotlin" % "kotlin-compiler" % "1.1.1",
  "org.scala-js" % "scalajs-ir_sjs0.6_2.12" % "0.6.15",
  /* Lib for tests */
  "org.scala-js" %% "scalajs-tools" % "0.6.15",
  "com.github.scopt" %% "scopt" % "3.5.0",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)

parallelExecution in Test := false