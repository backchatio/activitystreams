package io.backchat.jsonschema
package validator
package formats

import scalaz._
import Scalaz._
import Json._
import io.backchat.IPAddressParser

class Ipv6AddressFormatValidator extends Format {
  def apply(v1: JValue): Validation[ValidationError, JValue] = v1 match {
    case JString(v) if IPAddressParser.validIPv6Address(v) => v1.success
    case JString(v) => ValidationError("%s is not a valid IPv6 address.", "").fail
    case _ => ValidationError("IPv6 addresses can only be defined in strings.", "").fail
  }

  val key: String = "ipv6"

  val types: Set[Class[_ <: JValue]] = Set(classOf[JString])
}

