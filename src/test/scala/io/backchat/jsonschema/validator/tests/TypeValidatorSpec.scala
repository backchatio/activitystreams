package io.backchat.jsonschema
package validator
package tests

import com.codahale.jerkson.AST._
import io.backchat.SpecificationBase
import org.specs2.execute.Result
import Json._

trait ValidatorSpec { self: SpecificationBase =>

  def className: String
  def validator: SchemaValidator
  def file: String

  def is =
      "A "+className+" should" ^
        "succeeds for valid syntax" ! validatesSyntax ^
        "fails for valid syntax" ! validatesInvalidSyntax ^
        "when validates data" ! validatesData ^
      end

  def validatesSyntax = {
    val JArray(eles) = parse[JArray](getClass.getResourceAsStream(file))
    eles map (testSyntax(_)) reduce (_ and _)
  }

  def validatesInvalidSyntax = {
    validator.validateSyntax(JBoolean(value = true)) must beFailure
  }

  def validatesData = {
    val JArray(eles) = parse[JArray](getClass.getResourceAsStream(file))
    eles map (testEle(_)) reduce (_ and _)
  }


  def testEle(ele: JValue): Result = {
    val r = if ((ele \ "valid").valueAs[Boolean])
      validator.validateValue("data", ele, ele \ "schema") must beSuccess
    else
      validator.validateValue("data", ele, ele \ "schema") must beFailure
    if (r.isFailure) {
      r.mapMessage(_ + " source: "+generate(ele))
    } else r
  }

  def testSyntax(ele: JValue): Result = validator.validateSyntax(ele \ "schema") must beSuccess

}
class TypeValidatorSpec extends SpecificationBase with ValidatorSpec {


  val className: String = "TypeValidator"

  val validator: SchemaValidator = new TypeValidator
  val file = "/keyword/type.json"


}
