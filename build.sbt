name := "YetAnotherScalaGameOfLife"

version := "1.0.1"

scalaVersion := "2.10.3"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

//libraryDependencies ++= Seq(
//  ("com.typesafe.akka" %% "akka-actor" % "2.3.2").
//    exclude("com.typesafe", "config").
//    exclude("org.scala-lang", "scala-swing")
//)

libraryDependencies ++= Seq(
  ("org.scala-lang" % "scala-swing" % "2.10.2")//.
//    exclude("com.typesafe", "akka-actor")
)

