name := "expressions-algebraic-scala"

version := "0.0.1"

scalaVersion := "2.11.8"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-language:higherKinds")

libraryDependencies ++= Seq(
  "org.scalaz"     %% "scalaz-core"           % "7.2.8",
  "com.slamdata"   %% "matryoshka-core"       % "0.16.4",
  "com.slamdata"   %% "matryoshka-scalacheck" % "0.16.4" % Test
)
