package io.backchat.jsonschema
package validator
package formats

import scalaz._
import Scalaz._
import com.codahale.jerkson.AST._
import Json._
import io.backchat.EmailAddressParser

class EmailFormatValidator extends Format {

  // In the java version the author claims that it isn't specified that an email address
  // needs to have a domain part. But according to this http://tools.ietf.org/html/rfc2822#section-3.4.1
  // it really must have a domain part.

  def apply(v1: JValue): Validation[ValidationError, JValue] = v1 match {
    case JString(s) if EmailAddressParser.isValidMailbox(s) => v1.success
    case JString(s) => ValidationError("%s is not a valid email address", "").fail
    case _ => ValidationError("Only strings are allowed for email addresses", "").fail
  }

  val key: String = "email"

  val types: Set[Class[_ <: JValue]] = Set(classOf[JString])
}
