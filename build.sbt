name := "predictabilityParsing"

version := "0.02-SNAPSHOT"

scalaVersion := "2.9.2"

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
  "org.scalatest" % "scalatest_2.9.0" % "1.6.1" % "test",
  "org.scalala" % "scalala_2.9.1" % "1.0.0.RC2"
)

publishArtifact in packageDoc := false

