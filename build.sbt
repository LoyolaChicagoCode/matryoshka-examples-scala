name := "matryoshka-examples-scala"

version := "0.0.1"

scalaVersion := "2.12.1"

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-language:higherKinds",
  "-Ypartial-unification"
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies ++= Seq(
  "org.scalaz"     %% "scalaz-core"           % "7.2.8",
  "com.slamdata"   %% "matryoshka-core"       % "0.16.5",
  "com.slamdata"   %% "matryoshka-scalacheck" % "0.16.5" % Test
)
