enablePlugins(ScalaJSPlugin)
scalaVersion := "2.11.8"

lazy val trollish = crossProject.settings(
  organization := "com.github.lettenj61",
  name := "trollish",
  version := "0.0.3-SNAPSHOT",
  scalaVersion := "2.11.8",

  libraryDependencies ++= Seq(
    "com.lihaoyi" %%% "utest" % "0.4.3" % "test"
  ),
  testFrameworks += new TestFramework("utest.runner.Framework"))
.jvmSettings()
.jsSettings(
  scalaJSUseRhino in Global := false,
  libraryDependencies ++= Seq(
    "org.scala-js" %%% "scalajs-dom" % "0.8.2",
    "com.lihaoyi" %%% "scalatags" % "0.5.5"
  )
)

lazy val trollishJVM = trollish.jvm
lazy val trollishJS = trollish.js
