enablePlugins(ScalaJSPlugin, WorkbenchPlugin)

name := "DoisMil"

version := "0.1"

scalaVersion := "2.12.7"

//scalaJSUseMainModuleInitializer := true

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.6"

libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.6.7"