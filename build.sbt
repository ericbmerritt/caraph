name := "caraph"

organization := "caraph"

version := "0.0.1"

scalaVersion := "2.11.8"

scalacOptions := Seq("-unchecked",
  "-deprecation",
  "-encoding",
  "utf8",
  "-feature",
  "-Xfatal-warnings",
  "-Xcheckinit",
  "-Xlint")
scalacOptions in Test ++= Seq("-Yrangepos")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.5" % "test" withSources() withJavadoc(),
  "org.scalacheck" %% "scalacheck" % "1.13.3" % "test" withSources() withJavadoc()
)

initialCommands := "import caraph.caraph._"

