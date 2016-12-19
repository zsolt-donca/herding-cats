organization := "org.zsd"
name := "herding-cats"
scalaVersion := "2.11.8"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")

libraryDependencies += "org.typelevel" %% "cats" % "0.8.1"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"

//libraryDependencies += "com.slamdata" %% "matryoshka-core" % "0.15.1"

libraryDependencies += "org.specs2" %% "specs2-core" % "3.8.6" % "test"
libraryDependencies += "org.specs2" %% "specs2-scalacheck" % "3.8.6" % "test"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"

scalacOptions in Test += "-Yrangepos"
