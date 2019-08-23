name := "copy-util"

version := "0.1"

scalaVersion := "2.12.9"

libraryDependencies += "org.typelevel" %% "cats-core" % "1.6.1" withSources()
libraryDependencies += "org.typelevel" %% "cats-effect" % "1.3.1" withSources()
libraryDependencies += "com.beachape" %% "enumeratum" % "1.5.13" withSources()


scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-unchecked",
  "-language:postfixOps",
  "-language:higherKinds",
  "-Ypartial-unification")
