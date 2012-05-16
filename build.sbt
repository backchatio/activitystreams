name := "activitystreams"

version := "1.3.4-SNAPSHOT"

organization := "io.backchat.activitystreams"

scalaVersion := "2.9.1"

scalacOptions ++= Seq("-optimize", "-unchecked", "-deprecation", "-Xcheckinit", "-encoding", "utf8")

libraryDependencies ++= Seq(
  "io.backchat.jerkson" %% "jerkson" % "0.7.0-SNAPSHOT",
  "org.mozilla" % "rhino" % "1.7R3",
  "javax.mail" % "mailapi" % "1.4.3",
  "com.googlecode.libphonenumber" % "libphonenumber" % "4.8",
  "joda-time" % "joda-time" % "2.1",
  "com.google.guava" % "guava" % "12.0",
  "org.parboiled" % "parboiled-java" % "1.0.2",
  "org.slf4j" % "slf4j-api" % "1.6.4",
  "org.slf4j" % "log4j-over-slf4j" % "1.6.4",
  "org.slf4j" % "jcl-over-slf4j" % "1.6.4",
  "ch.qos.logback" % "logback-classic" % "1.0.3" % "provided",
  "org.specs2" %% "specs2" % "1.10" % "test",
  "junit" % "junit" % "4.10" % "test",
  "org.testng" % "testng" % "6.3.1" % "test"
)

resolvers += "Sonatype Nexus Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

testOptions := Seq(
        Tests.Argument("console", "junitxml"))
        
testOptions <+= crossTarget map { ct =>
  Tests.Setup { () => System.setProperty("specs2.junit.outDir", new File(ct, "specs-reports").getAbsolutePath) }
}

parallelExecution in Test := false

compileOrder := CompileOrder.JavaThenScala

seq(testNGSettings:_*)