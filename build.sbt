

lazy val commonSettings = Seq(
  organization := "com.example",
  version := "0.1.0",
  scalaVersion := "2.11.6"
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "simplesymbols",
    libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
  )

mainClass in (root, Compile) := Some("simplesymbols.ConsoleApp")
