name := "activitystreams"

version := "0.5.1-SNAPSHOT"

organization := "io.backchat.activitystreams"

scalaVersion := "2.9.1"

scalacOptions ++= Seq("-optimize", "-unchecked", "-deprecation", "-Xcheckinit", "-encoding", "utf8")

libraryDependencies ++= Seq(
  "io.backchat.jerkson"    %% "jerkson"            % "0.7.0-SNAPSHOT",
  "io.backchat.rl"         %% "rl"                 % "0.3.1",
  "io.backchat.inflector"  %% "scala-inflector"    % "1.3.3",
  "org.scalaz"              % "scalaz-core_2.9.1"  % "6.0.4",
  "org.mozilla"             % "rhino"              % "1.7R3",
  "javax.mail"              % "mail"               % "1.4.5",
  "commons-codec"           % "commons-codec"      % "1.6",
  "commons-validator"       % "commons-validator"  % "1.4.0",
  "com.googlecode.libphonenumber" % "libphonenumber" % "4.8",
  "com.google.guava"        % "guava"              % "12.0",
  "net.databinder"         %% "dispatch-http"      % "0.8.7",
  "org.slf4j"               % "slf4j-api"          % "1.6.4",
  "org.scala-tools.time"   %% "time"               % "0.5",
  "org.slf4j"               % "log4j-over-slf4j"   % "1.6.4" % "provided",
  "org.slf4j"               % "jcl-over-slf4j"     % "1.6.4" % "provided",
  "ch.qos.logback"          % "logback-classic"    % "1.0.3" % "provided",
  "org.specs2"             %% "specs2"             % "1.10"  % "test",
  "junit"                   % "junit"              % "4.10"  % "test"
)

resolvers += "Sonatype Nexus Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

resolvers += "Sonatype Nexus Releases" at "https://oss.sonatype.org/content/repositories/releases"

testOptions := Seq(
        Tests.Argument("console", "junitxml"))
        
testOptions <+= crossTarget map { ct =>
  Tests.Setup { () => System.setProperty("specs2.junit.outDir", new File(ct, "specs-reports").getAbsolutePath) }
}

parallelExecution in Test := false

compileOrder := CompileOrder.JavaThenScala
