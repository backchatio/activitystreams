name := "activitystreams"

version := "1.3.4-SNAPSHOT"

organization := "io.backchat.activitystreams"

scalaVersion := "2.9.1"

scalacOptions ++= Seq("-optimize", "-unchecked", "-deprecation", "-Xcheckinit", "-encoding", "utf8")

libraryDependencies ++= Seq(
  "io.backchat.jerkson" %% "jerkson" % "0.7.0-SNAPSHOT",
  "org.specs2" % "specs2_2.9.1" % "1.9" % "test"
)

resolvers += "Sonatype Nexus Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

testOptions := Seq(
        Tests.Argument("console", "junitxml"))
        
testOptions <+= crossTarget map { ct =>
  Tests.Setup { () => System.setProperty("specs2.junit.outDir", new File(ct, "specs-reports").getAbsolutePath) }
}

parallelExecution in Test := false