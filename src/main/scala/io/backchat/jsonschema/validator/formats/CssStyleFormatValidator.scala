package io.backchat.jsonschema
package validator
package formats

import scalaz._
import Scalaz._
import Json._

object CssStyleFormatValidator {
  private val styleElement = """(?i)\s*[^:]+\s*:\s*[^;]+""".r
  private val splitPattern = """\s*;\s*""".r
}
class CssStyleFormatValidator extends Format {
  import CssStyleFormatValidator._
  val key: String = "style"
  def apply(v1: JValue): Validation[ValidationError, JValue] = v1 match {
    case JString(v) if isValid(v) => v1.success
    case JString(v) => ValidationError("%s is not a valid CSS 2.1 style." % v, "").fail
    case _ => ValidationError("Only strings are allowed for CSS styles", "").fail
  }

  def isValid(v: String): Boolean = {
    val lst = splitPattern.split(v)
    lst.nonEmpty && (lst forall (styleElement.findFirstIn(_).isDefined))
  }

  val types: Set[Class[_ <: JValue]] = Set(classOf[JString])
}
