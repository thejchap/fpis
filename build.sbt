ThisBuild / scalaVersion := "2.12.7"

lazy val fpis = (project in file("."))
  .settings(
    name := "FPIS",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test,
  )