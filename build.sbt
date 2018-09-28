name := "matryoshka-examples-scala"

version := "0.2"

scalaVersion := "2.12.7"

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
  "org.scalaz"     %% "scalaz-core"           % "7.2.26",
  "com.slamdata"   %% "matryoshka-core"       % "0.21.3",
  "com.slamdata"   %% "matryoshka-scalacheck" % "0.21.3" % Test
)
