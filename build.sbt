lazy val root = (project in file("."))


scalaVersion := "2.11.1"


libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"

unmanagedBase := baseDirectory.value / "lib"