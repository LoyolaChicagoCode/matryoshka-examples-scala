name := "matryoshka-examples-scala"

version := "0.2"

scalaVersion := "2.12.16"

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-language:higherKinds",
  "-Ypartial-unification"
)

addCompilerPlugin("org.typelevel" % "kind-projector_2.12.16" % "0.13.2")

libraryDependencies ++= Seq(
  "org.scalaz"     %% "scalaz-core"           % "7.3.6",
  "com.slamdata"   %% "matryoshka-core"       % "0.21.3",
  "org.scalaz"     %% "scalaz-scalacheck-binding" % "7.3.6" % Test,
  "com.slamdata"   %% "matryoshka-scalacheck"     % "0.21.3" % Test
)
