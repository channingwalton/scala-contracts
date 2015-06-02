name := "scala-contracts"

version := "0.0.1"

scalaVersion := "2.11.6"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

scalacOptions ++= Seq(
  "-feature",
  "-language:_",
  "-Xfatal-warnings",
  "-deprecation",
  "-unchecked")