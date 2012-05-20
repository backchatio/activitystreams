package io.backchat.jsonschema
package validator.tests

import io.backchat.SpecificationBase
import validator.{DependenciesValidator, SchemaValidator}
import org.specs2.execute.Result

class SimpleDependenciesSpec extends SpecificationBase with ValidatorSpec {

  val className: String = "DependenciesValidator"

  val validator: SchemaValidator = new DependenciesValidator

  val file: String = "/keyword/dependenciesSimple.json"

  override def validatesInvalidSyntax: Result = success
}
