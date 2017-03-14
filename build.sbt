name := "Kotlin-Scalajs"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.jetbrains.kotlin" % "kotlin-compiler" % "1.1.0",
  "org.scala-js" % "scalajs-ir_sjs0.6_2.11" % "0.6.14"
)
