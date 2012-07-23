name := "Squealer"

version := "0.1-SNAPSHOT"

scalaVersion := "2.9.2"

resolvers ++= Seq(
  "snapshots" at "http://scala-tools.org/repo-snapshots",
  "releases" at "http://scala-tools.org/repo-releases",
  "sonatype" at "https://oss.sonatype.org/content/groups/public"
)

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "1.11" % "test",
  "org.hsqldb" % "hsqldb" % "2.2.6" % "test",
  "com.eed3si9n" %% "treehugger" % "0.1.4-SNAPSHOT",
  "org.skife.com.typesafe.config" % "typesafe-config" % "0.3.0"
)

mainClass in (Compile, run) := Some("com.wheaties.squealer.Main")