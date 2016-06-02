enablePlugins(ScalaJSPlugin)

name := "trollish"
version := "0.0.3-SNAPSHOT"
scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "com.lihaoyi" %%% "utest" % "0.4.3" % "test"
)
testFrameworks += new TestFramework("utest.runner.Framework")
