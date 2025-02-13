val scala3Version = "3.6.2"

scalacOptions ++= Seq(
  "-Xlint:unused",
  "-Xlint:infer-any",
  "-Wunused:linted",
  "-unchecked"
)

lazy val root = project
  .in(file("."))
  .settings(
    name := "dcd",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.10",
    libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4",

    libraryDependencies += "com.olvind.tui" %% "tui" % "0.0.7",
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test
  )
