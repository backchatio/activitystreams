package io.backchat.jsonschema
package validator
package formats

import scalaz._
import Scalaz._
import com.codahale.jerkson.AST._
import Json._
import java.net.{URI, URISyntaxException}

class URIFormatValidator extends Format {


  val key: String = "uri"

  def apply(v1: JValue): Validation[ValidationError, JValue] = v1 match {
    case JString(s) if isValid(s) => v1.success
    case JString(s) => ValidationError("%s is not a valid URI." % s, "").fail
    case _ => ValidationError("URI's can only be strings", "").fail
  }

  import util.control.Exception._
  def isValid(s: String) = (catching(classOf[URISyntaxException]) opt { new URI(s) }).isDefined

  val types: Set[Class[_ <: JValue]] = Set(classOf[JString])
}
