import AssemblyKeys._ // put this at the top of the file

assemblySettings

// your assembly settings here
//jarName in assembly :=
//test in assembly :=
//mainClass in assembly :=
//outputPath in assembly :=
//mergeStrategy in assembly :=
//assemblyOption in assembly :=
//excludedJars in assembly :=
//assembledMappings in assembly :=

//jarName in assembly := "something.jar"

//excludedJars in assembly <<= (fullClasspath in assembly) map { cp =>
//  cp filter {_.data.getName == "akka-actor_2.10-2.3.2.jar"}
//}