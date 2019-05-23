organization := "com.dream"
name := "prndeitor"

scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.5.22"
)

licenses := Seq(("CC0", url("http://creativecommons.org/publicdomain/zero/1.0")))

enablePlugins(JavaAppPackaging)