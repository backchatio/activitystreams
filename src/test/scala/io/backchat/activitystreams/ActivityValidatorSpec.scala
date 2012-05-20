package io.backchat.activitystreams

import org.specs2.Specification
import org.eel.kitchen.util.JsonLoader
import org.eel.kitchen.jsonschema.main.ValidationReport
import collection.JavaConverters._
import org.eel.kitchen.jsonschema.schema.{SyntaxValidator, JsonSchema}

class ActivityValidatorSpec extends Specification { def is = sequential ^
  "An ActivityValidator should" ^
    "pass validation for a valid tweet activity" ! validatesValid("tweet") ^
//    "fail validation for a invalid [no_actor] tweet activity" ! validatesInvalid("tweet", "no_actor") ^
//    "fail validation for a invalid [no_object] tweet activity" ! validatesInvalid("tweet", "no_object") ^
//    "fail validation for a invalid [no_verb] tweet activity" ! validatesInvalid("tweet", "no_verb") ^
//    "fail validation for a invalid [no_object_id] tweet activity" ! validatesInvalid("tweet", "no_object_id") ^
  end


  def validatesValid(kind: String) = {
    val schemaNode = JsonLoader.fromPath("./activitystreams/activity.json")
    val json = JsonLoader.fromResource("/valid_%s.json" format kind)
    val schema = JsonSchema.fromNode(schemaNode)
    val syntax = new ValidationReport
    SyntaxValidator.validate(syntax, schemaNode)
    syntax.isSuccess must beTrue and {
      val report = new ValidationReport
      schema.validate(report, json)
      println("past the actual test: %s" format report.getMessages.asScala.toList.mkString(", "))
      report.isSuccess must beTrue
    }
  }

  def validatesInvalid(kind: String, mistake: String) = {
    val schemaNode = JsonLoader.fromPath("./activitystreams/activity.json")
    val json = JsonLoader.fromResource("/invalid_%s_%s.json" format (kind, mistake))
    val schema = JsonSchema.fromNode(schemaNode)
    val syntax = new ValidationReport
    SyntaxValidator.validate(syntax, schemaNode)
    syntax.isSuccess must beTrue and {
      val report = new ValidationReport
      schema.validate(report, json)
      println("past the actual test: %s" format report.getMessages.asScala.toList.mkString(", "))
      report.isSuccess must beFalse
    }
  }

}
