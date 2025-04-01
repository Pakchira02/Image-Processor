name := "image_processor"

version := "0.1"

scalaVersion := "2.13.12"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
  "org.scalanlp" %% "breeze" % "2.1.0", // for mathematical operations
  "org.scalatest" %% "scalatest" % "3.2.16" % Test // for unit testing
)

