lazy val root = (project in file("."))
  .settings(
    name := "bus",
    scalaVersion := "3.3.0",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1",
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.16",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.16" % "test",
  )

Compile / scalaSource := baseDirectory.value / "src"

Test / scalaSource := baseDirectory.value / "test"
