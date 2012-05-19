package io.backchat.jsonschema
package validator
package formats

import scalaz._
import Scalaz._
import Json._
import com.google.common.net.InternetDomainName

class HostnameFormatValidator extends Format {
  def apply(v1: JValue): Validation[ValidationError, JValue] = v1 match {
    case JString(v) if InternetDomainName.isValid(v) => v1.success
    case JString(v) => ValidationError("%s is not a valid host name.", "").fail
    case _ => ValidationError("Host names can only be defined in strings.", "").fail

  }

  val key: String = "host-name"

  val types: Set[Class[_ <: JValue]] = Set(classOf[JString])
}
