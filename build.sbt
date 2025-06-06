val scala3Version = "3.7.0"

scalacOptions ++= Seq(
  "-Xlint:unused",
  "-Xlint:infer-any",
  "-Wunused:linted",
  "-unchecked",
  "-language:strictEquality"
)

lazy val root = project
  .in(file("."))
  .settings(
    name := "dcd",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "com.olvind.tui" %% "tui" % "0.0.7",
    libraryDependencies += "com.lihaoyi" %% "upickle" % "4.1.0",
    libraryDependencies += "org.jfree" % "jfreechart" % "1.5.5",
    libraryDependencies += "com.github.librepdf" % "openpdf" % "2.0.3",
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test
  )
