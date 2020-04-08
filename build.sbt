name := "catbird-rerunnable-tests"
homepage := Option(url("https://github.com/bpholt/catbird-rerunnable-tests"))

scalaVersion := "2.12.11"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.1.1",
  "org.typelevel" %% "alleycats-core" % "2.1.1",
  "io.catbird" %% "catbird-finagle" % "20.3.0",
  "io.catbird" %% "catbird-util" % "20.3.0",
  "org.typelevel" %% "cats-tagless-core" % "0.11",
  "org.typelevel" %% "cats-tagless-macros" % "0.11",
  "org.typelevel" %% "cats-effect" % "2.1.2",
  "org.scalatest" %% "scalatest" % "3.1.1" % Test,
  "org.scalatestplus" %% "scalacheck-1-14" % "3.1.1.1" % Test,
)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)

val app = (project in file("."))
