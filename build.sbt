name := "wollok-s"

scalaVersion := "2.12.3"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.5"
//libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.4" % "test"
//libraryDependencies += "org.scala-lang.modules" %%% "scala-parser-combinators" % "1.0.5"

//enablePlugins(ScalaJSPlugin)

scalacOptions := Seq("-deprecation", "-unchecked", "-feature", "-Ypatmat-exhaust-depth", "1600")