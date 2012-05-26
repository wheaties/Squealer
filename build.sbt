name := "Squealer"

version := "0.1-SNAPSHOT"

scalaVersion := "2.9.1"

resolvers ++= Seq(
  "snapshots" at "http://scala-tools.org/repo-snapshots",
  "releases" at "http://scala-tools.org/repo-releases",
  "sonatype" at "https://oss.sonatype.org/content/groups/public",
  "maxaf_snap" at "http://maxaf.github.com/repo/snapshots/",
  "maxaf_release" at "http://maxaf.github.com/repo/releases/"
)

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "1.7" % "test",
  "org.hsqldb" % "hsqldb" % "2.2.6" % "test",
  "com.eed3si9n" %% "treehugger" % "0.1.2",
  "org.skife.com.typesafe.config" % "typesafe-config" % "0.3.0",
  "seekwell" %% "seekwell" % "0.0.1-SNAPSHOT"
)
