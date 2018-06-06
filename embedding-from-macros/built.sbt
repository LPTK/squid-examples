name := "relation-dsl"

version := "1.0.1"

scalaVersion := "2.12.6"

resolvers ++= Seq(Resolver.sonatypeRepo("releases"),
      Resolver.sonatypeRepo("snapshots"))

libraryDependencies += "ch.epfl.data" %% "squid" % "0.3.0-SNAPSHOT"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
