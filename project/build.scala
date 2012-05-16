import sbt._
import Keys._

object ActivityStreamsBuild extends Build {
  val dispatchLiftJson =
         uri("git://github.com/mojolly/dispatch-lift-json")
  val root = Project("activitystreams", file(".")) dependsOn(dispatchLiftJson)
}