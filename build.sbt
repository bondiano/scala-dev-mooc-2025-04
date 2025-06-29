lazy val root = (project in file("."))
  .aggregate(scala213Module, scala3Module)

lazy val scala213Module = (project in file("scala213Module"))
  .settings(
    name := "Scala 2.13 Module",
    scalaVersion := "2.13.16",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.10.0",
      "org.scalatest" %% "scalatest" % "3.2.18" % Test,
      "dev.zio" %% "zio" % "2.1.19"
    )
  )

lazy val scala3Module = (project in file("scala3Module"))
  .settings(
    name := "Scala 3 Module",
    scalaVersion := "3.3.6"
    // Add other settings and dependencies specific to this module
  )
