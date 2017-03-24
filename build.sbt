name := "Kotlin-Scalajs"

version := "0.1-SNAPSHOT"

scalaVersion := "2.12.1"

libraryDependencies ++= Seq(
  "org.jetbrains.kotlin" % "kotlin-compiler" % "1.1.1",
  "org.scala-js" % "scalajs-ir_sjs0.6_2.12" % "0.6.15",
  "org.scala-js" %% "scalajs-tools" % "0.6.15" % Test,
  "com.github.scopt" %% "scopt" % "3.5.0" % Test,
  "org.scalatest" %% "scalatest" % "3.0.1" % Test
)

parallelExecution in Test := false
