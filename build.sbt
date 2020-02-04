name := "bitml_scala_api"

version := "0.1"

scalaVersion := "2.11.12"

libraryDependencies += "fr.acinq" %% "bitcoin-lib" % "0.16"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.1.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0" % "test"
libraryDependencies += "org.json4s" %% "json4s-native" % "3.6.7"
libraryDependencies += "org.json4s" %% "json4s-ext" % "3.6.7"
libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.5.29"
libraryDependencies += "com.typesafe.akka" %% "akka-remote" % "2.5.29"
libraryDependencies += "com.typesafe.akka" %% "akka-testkit" % "2.5.29" % Test


