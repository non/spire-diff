name := "spire-diff"

version := "0.0.3"

scalaVersion := "2.11.4"

libraryDependencies ++= (
  "org.spire-math" %% "spire" % "0.8.2" ::
  "org.scalatest"  %% "scalatest" % "2.2.1" % "test" ::
  "org.scalacheck" %% "scalacheck" % "1.12.1" % "test" ::
  Nil
)

scalacOptions ++= (
  "-feature" ::
  "-language:_" ::
  "-deprecation" ::
  "-Xexperimental" ::
  Nil
)
