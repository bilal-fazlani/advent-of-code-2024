val scala3Version = "3.5.2"
val zioVersion = "2.1.6"

lazy val root = project
  .in(file("."))
  .settings(
    name := "advent-of-code-2024",
    scalaVersion := scala3Version,
    scalacOptions ++= Seq(
      "-source:3.4-migration",
      "-rewrite",
      "-unchecked",
      "-feature"
    ),
    libraryDependencies ++= Seq(
      "com.bilal-fazlani" %% "rainbowcli" % "3.0.1",
      "org.scalameta" %% "munit" % "0.7.29" % Test
    )
  )
