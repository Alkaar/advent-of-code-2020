name := "advent-of-code-2020"

scalaVersion := "2.13.4"

libraryDependencies ++= Seq(
  "org.typelevel"  %% "cats-core"     % "2.1.1",
  "com.beachape"   %% "enumeratum"    % "1.5.13",
  "org.scala-lang" % "scala-compiler" % scalaVersion.value
)
