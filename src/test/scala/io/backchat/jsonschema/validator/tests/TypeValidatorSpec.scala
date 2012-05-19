package io.backchat.jsonschema
package validator
package tests

import io.backchat.SpecificationBase
import org.specs2.execute.Result
import Json._
import org.specs2.specification.Fragments

trait ValidatorSpec { self: SpecificationBase =>

  def className: String
  def validator: SchemaValidator
  def file: String

  def is =
      "A "+className+" should" ^
        "when validating syntax" ^ validatesSyntax ^
        "fails for invalid syntax" ! validatesInvalidSyntax ^ bt ^
        "when validating data" ^ validatesData ^
      end

  def validatesSyntax = {
    val JArray(eles) = parse[JArray](getClass.getResourceAsStream(file))
    eles map { e =>
      val schema = e \ "schema"
      ("succeeds for %s" % generate(schema) ! { validator.validateSyntax(schema) must beSuccess }) : Fragments
    } reduce (_ ^ _)
  }

  def validatesInvalidSyntax = {
    validator.validateSyntax(JBoolean(value = true)) must beFailure
  }

  def validatesData = {
    val JArray(eles) = parse[JArray](getClass.getResourceAsStream(file))
    eles map { e =>
      val valid = (e \ "valid").valueAs[Boolean]
      val schema = e \ "schema"
      val data = generate(e \ "data")
      "%s validation for %s with schema %s".format(if (valid) "passes" else "fails", data, generate(schema)) ! {
        if (valid) validator.validateValue("data", e, schema) must beSuccess
        else validator.validateValue("data", e, schema) must beFailure
      } : Fragments
    } reduce (_ ^ _)
  }

  def testSyntax(ele: JValue): Result = validator.validateSyntax(ele \ "schema") must beSuccess

}
class TypeValidatorSpec extends SpecificationBase with ValidatorSpec {


  val className: String = "TypeValidator"

  val validator: SchemaValidator = new TypeValidator
  val file = "/keyword/type.json"


}
