package io.backchat.jsonschema

import java.net.{URL, URI}
import dispatch._
import java.io.File
import scalaz._
import Scalaz._
import validator.Validators
import xml.pull.XMLEventReader
import Json._


object JsonSchema extends  Validators {

  object Mode extends Enumeration {
    val Core, Hyper = Value
  }
  def apply(schema: URI): JsonSchema = {
    require(schema.isAbsolute, "The schema URI needs to be absolute.")
    new JsonSchema(JObject(JField("$schema", JString(schema.toASCIIString)) :: Nil))
  }
  def apply(schema: File): JsonSchema = {
    require(schema.getAbsoluteFile.exists(), "Couldn't find the schema at: "+schema.getPath)
    apply(schema.getAbsoluteFile.toURI)
  }
  def apply(schema: URL): JsonSchema = {
    require(fetchUrl(schema) != null, "Couldn't find a schema at: "+schema.toString)
    apply(schema.toURI)
  }

  import util.control.Exception.allCatch
  private[this] def fetchUrl(toFetch: URL) = {
    val http = new Http
    allCatch.withApply(_ => null).andFinally(http.shutdown()) {
      (http when `status is 202 or 204`)(url(toFetch.toURI.toASCIIString).HEAD >|)
      ""
    }
  }

  val `202 or 204` = List(202, 204)
  def `status is 202 or 204`(code: Int) = `202 or 204` contains code


}

/**
 * The base url is used to determine which strategy to use when trying to resolve a schema
 * @param baseUrl
 */
class JsonSchema(val baseSchema: JValue = JUndefined) {

  import AsJValue._
  import JsonSchema._
  private implicit val _this = this

  def validate(data: JValue): ValidationNEL[ValidationError, JValue] = {

    data.success
  }

  def \(name: String): JValue = baseSchema \ name

  def \\(name: String): Seq[JValue] = baseSchema \\ name




//  def validate[T : AsJValue](json: T): ValidationNEL[ValidationError, JValue] = {
//    val jvalue = implicitly[AsJValue[T]].asJValue(json)
//    jvalue match {
//      case m: JObject => validateObject(m, implicitly[AsJValue[URI]].asJValue(node))
//    }
//    null
//  }
//
//  private def validateObject(json: JObject, schema: JValue)



}
