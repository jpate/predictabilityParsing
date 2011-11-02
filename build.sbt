name := "predictabilityParsing"

version := "0.01"

scalaVersion := "2.9.1"

libraryDependencies += "org.scalatest" % "scalatest_2.9.0" % "1.6.1" % "test"

libraryDependencies += "junit" % "junit" % "4.8" % "test"

unmanagedClasspath in Compile += Attributed.blank(new java.io.File("doesnotexist"))

// traceLevel := 100

scalacOptions += "-deprecation"

