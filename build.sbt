name := "predictabilityParsing"

version := "0.021-SNAPSHOT"

scalaVersion := "2.10.0"

unmanagedClasspath in Compile += Attributed.blank(new java.io.File("doesnotexist"))

// traceLevel := 100

scalacOptions += "-deprecation"

resolvers ++= Seq(
  // other resolvers here
  "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/",
  "ScalaNLP Maven2" at "http://repo.scalanlp.org/repo"
)

libraryDependencies  ++= Seq(
  "junit" % "junit" % "4.8" % "test",
  "org.scalatest" % "scalatest_2.10.0" % "1.8" % "test"
)

publishArtifact in packageDoc := false

