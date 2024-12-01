val scala3Version = "3.5.2"
val zioVersion = "2.1.6"

lazy val root = project
  .in(file("."))
  .settings(
    name := "advent-of-code-2024",
    scalaVersion := scala3Version,
    scalacOptions ++= Seq(
      "-source:3.2-migration",
      "-rewrite",
      "-unchecked",
      "-feature"
    ),
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % zioVersion,
      "dev.zio" %% "zio-streams" % zioVersion,
      "dev.zio" %% "zio-parser" % "0.1.10",
      "dev.zio" %% "zio-test" % zioVersion % Test,
      "dev.zio" %% "zio-test-sbt" % zioVersion % Test
    )
  )
