package io.backchat.jsonschema
package validator
package formats

import scalaz._
import Scalaz._
import Json._
import com.google.i18n.phonenumbers.{NumberParseException, PhoneNumberUtil}

class PhoneNumberFormatValidator extends Format {
  val key: String = "phone"
  def apply(v1: JValue): Validation[ValidationError, JValue] = v1 match {
    case JString(ph) if isValid(ph) => v1.success
    case JString(ph) => ValidationError("%s is an invalid phone number" % ph, "").fail
    case m => ValidationError("Only strings are allowed for phone numbers.", "").fail
  }

  import util.control.Exception._
  def isValid(v: String): Boolean = {
    val u = PhoneNumberUtil.getInstance
    catching(classOf[NumberParseException]).withApply(_ => false) {
      if (v startsWith "+") u.parse(v, "ZZ") else u.parse(v, "GB")
      true
    }
  }

  val types: Set[Class[_ <: JValue]] = Set(classOf[JString])
}
