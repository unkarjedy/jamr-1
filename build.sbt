import AssemblyKeys._

assemblySettings

name := "jamr"

version := "0.1-SNAPSHOT"

organization := "edu.cmu.lti.nlp"

scalaVersion := "2.10.3"

libraryDependencies ++= Seq(
  "com.jsuereth" %% "scala-arm" % "1.3",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.4.1" withSources() withJavadoc(),
  "edu.stanford.nlp" % "stanford-corenlp" % "3.4.1" classifier "models",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.4.1",
  "org.scala-lang.modules" %% "scala-pickling" % "0.10.0",
  "commons-io" % "commons-io" % "2.5",
  "org.apache.commons" % "commons-csv" % "1.3",
  "org.apache.commons" % "commons-lang3" % "3.0"
//  "org.scala-lang" % "scala-swing" % "2.10.3"
)

//scalaSource in compile := (baseDirectory in compile).value  / "src"

scalaSource in Compile := baseDirectory.value / "src"

// Running JAMR via sbt:

fork in run := true  // run in separate JVM than sbt

connectInput in run := true

outputStrategy in run := Some(StdoutOutput)  // connect to stdin/stdout/stderr of sbt's process

logLevel in run := Level.Error  // don't clutter stdout with [info]s

ivyLoggingLevel in run := UpdateLogging.Quiet

traceLevel in run := 0

javaOptions in run ++= Seq(
  "-Xmx8g",
  "-XX:MaxPermSize=256m",
  "-ea",
  "-Dfile.encoding=UTF-8",
  "-XX:ParallelGCThreads=2"
)

mainClass := Some("edu.cmu.lti.nlp.amr.AMRParser")
