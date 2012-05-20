package io.backchat
package jsonschema
package validator
package tests

import org.specs2.matcher.MatchResult
import Json._

class AdditionalItemsValidatorSpec extends SpecificationBase with ValidatorSpec {

  val className: String = "AdditionalItemsValidator"

  val validator: SchemaValidator = new AdditionalItemsValidator

  val file: String = "/keyword/additionalItems.json"

  override def validatesInvalidSyntax = success
}
