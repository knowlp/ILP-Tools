name := "ILP-Tools"
version := "0.1"
organization := "nkatz"
scalaVersion := "2.11.6"


// MongoDB
libraryDependencies += "org.mongodb" %% "casbah" % "2.8.1"

// Scala-lang
libraryDependencies ++= Seq(
"org.scala-lang" % "scala-library" % scalaVersion.value,
"org.scala-lang" % "scala-reflect" % scalaVersion.value
)

// Scala-modules
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3"

// ScalaTest
libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.1.3" % "test"

// Akka lib
resolvers += "Akka Snapshot Repository" at "http://repo.akka.io/snapshots/"
libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.11"




