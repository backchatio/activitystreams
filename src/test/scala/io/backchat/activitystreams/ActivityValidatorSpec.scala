package io.backchat.activitystreams

import org.specs2.Specification
import scala.io.Source
import org.eel.kitchen.util.JsonLoader
import org.eel.kitchen.jsonschema.main.ValidationReport
import collection.JavaConverters._
import org.eel.kitchen.jsonschema.schema.{SyntaxValidator, JsonSchema}

class ActivityValidatorSpec extends Specification { def is = sequential ^
  "An ActivityValidator should" ^
    "validate a valid tweet activity" ! validatesValid("tweet") ^
//    "validate a valid webfeed activity" ! validatesValid("webfeed") ^
  end


  def validatesValid(kind: String) = {

    val schemaNode = JsonLoader.fromResource("/activitystreams/activity.json")
    val json = JsonLoader.fromResource("/valid_%s.json" format kind)
    val schema = JsonSchema.fromNode(schemaNode)
    val syntax = new ValidationReport
    SyntaxValidator.validate(syntax, schemaNode.get("properties"))
    println("syntax: "+syntax.getMessages.asScala.mkString(", "))
    syntax.isSuccess must beTrue and {
      val report = new ValidationReport
      schema.validate(report, json)
      println("past the actual test: %s" format report.getMessages.asScala.toList)
      report.isSuccess must beTrue
    }
  }

}
