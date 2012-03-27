name := "scala-contracts"

version := "0.0.1"

scalaVersion := "2.9.1"

testFrameworks ++= Seq(new TestFramework("org.specs2.runner.SpecsFramework"))

libraryDependencies ++= Seq (
    "org.specs2" %% "specs2" % "1.8.2" % "test"
)