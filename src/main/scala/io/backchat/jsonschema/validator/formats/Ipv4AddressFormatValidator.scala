package io.backchat.jsonschema
package validator
package formats

import scalaz._
import Scalaz._
import com.codahale.jerkson.AST._
import Json._
import io.backchat.IPAddressParser

class Ipv4AddressFormatValidator extends Format {
  def apply(v1: JValue): Validation[ValidationError, JValue] = v1 match {
    case JString(v) if IPAddressParser.validIPv4Address(v) => v1.success
    case JString(v) => ValidationError("%s is not a valid IPv4 address.", "").fail
    case _ => ValidationError("IPv4 addresses can only be defined in strings.", "").fail
  }

  val key: String = "ip-address"

  val types: Set[Class[_ <: JValue]] = Set(classOf[JString])
}
