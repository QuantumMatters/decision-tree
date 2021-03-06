val scala3Version = "3.1.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "decision-tree",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "0.7.29" % Test,
      "com.github.tototoshi" %% "scala-csv" % "1.3.10"
    )
  )
