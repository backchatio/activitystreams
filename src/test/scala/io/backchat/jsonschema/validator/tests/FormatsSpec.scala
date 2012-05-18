package io.backchat.jsonschema
package validator
package tests

import io.backchat.SpecificationBase
import org.specs2.specification.Fragments
import com.codahale.jerkson.AST._
import org.specs2.execute.{Failure, Pending}

class FormatsSpec extends SpecificationBase {
  def is: Fragments =
    "When dealing with formats" ^
      "invalid cases must be detected" ! invalidCases ^
      "detects invalid hostname" ! invalidHostname ^
      "ignores unknown format" ! pendingUntilSchema ^ bt ^
      addFormatSpecs ^
    end

  val json = Json.parse[JValue](getClass.getResourceAsStream("/format.json"))

  def invalidCases = {
    "style" :: "ip-address" :: "phone" :: "utc-millisec" :: "uri" :: "date" :: "date-time" :: "time" :: Nil map { fmt =>
      val definition = json \ fmt
      val data = definition \ "instance"
      val schema = JObject(JField("format", JString(fmt)) :: Nil)
      JsonSchema.formats(fmt)(data) must beFailure
    } reduce (_ and _)
  }

  def invalidHostname = {
//    val schema = JObject(JField("format", JString("host-name")) :: Nil)
//    JsonSchema.formats("host-name")(data) must beFailure
    pendingUntilSchema
  }

  def addFormatSpecs: Fragments = {
    JsonSchema.formats.keys map { fmt =>
      ("valdates with %s format" % fmt ^ validatesFormatFragments(fmt))
    } reduce (_ ^ bt ^ _)
  }

  def validatesFormatFragments(name: String) = {
    try {
      val JArray(items) = Json.parse[JValue](getClass.getResourceAsStream("/format/%s.json" % name))
      items map { jv =>
        val data = jv \ "data"
        val valid = (jv \ "valid").valueAs[Boolean]
        val msg = if (valid) "passes" else "fails"
        ("%s validation for %s with %s" % (msg, name, Json.generate(data)) ! validatesFormat(name, jv)): Fragments
      } reduce (_ ^ _)
    } catch {
      case _ => ("File was not found" ! Failure("The file for %s could not be found." % name)): Fragments
    }
  }

  def validatesFormat(name: String, jv: JValue) = {
    val data = jv \ "data"
    val valid = (jv \ "valid").valueAs[Boolean]
    val validateFormat = JsonSchema.formats(name)
    val res = validateFormat(data)
    if (valid) res must beSuccess
    else res must beFailure
  }

  def pendingUntilSchema = Pending("PENDING: needs a full schema implementation preferrably")
}
