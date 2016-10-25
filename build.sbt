name := "external-sorting"

version := "1.0"

organization := "org.viacheslav.rodionov"

scalaVersion := "2.10.5"

javacOptions ++= Seq("-source", "1.8", "-target", "1.8")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"