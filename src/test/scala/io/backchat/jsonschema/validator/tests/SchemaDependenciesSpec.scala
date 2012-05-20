package io.backchat.jsonschema
package validator.tests

import io.backchat.SpecificationBase
import validator.{DependenciesValidator, SchemaValidator}

class SchemaDependenciesSpec extends SpecificationBase with ValidatorSpec {

  val className: String = "DependenciesValidator"

  val validator: SchemaValidator = new DependenciesValidator

  val file: String = "/keyword/dependenciesSchema.json"
}