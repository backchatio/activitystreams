package io.backchat.jsonschema
package validator
package tests

import com.codahale.jerkson.Json._
import com.codahale.jerkson.AST._
import io.backchat.SpecificationBase
import org.specs2.execute.Result

class TypeValidatorSpec extends SpecificationBase {
  def is =
    "A TypeValidator should" ^
      "succeeds for valid syntax" ! validatesSyntax ^
      "fails for valid syntax" ! validatesInvalidSyntax ^
      "when validates data" ! validatesData ^
    end


  val validator = new TypeValidator

  def validatesSyntax = {
    val JArray(eles) = parse[JArray](getClass.getResourceAsStream("/keyword/type.json"))
    eles map (testSyntax(_)) reduce (_ and _)
  }

  def validatesInvalidSyntax = {
    validator.validateSyntax(JBoolean(value = true)) must beFailure
  }

  def validatesData = {
    val JArray(eles) = parse[JArray](getClass.getResourceAsStream("/keyword/type.json"))
    eles map (testEle(_)) reduce (_ and _)
  }


  def testEle(ele: JValue): Result = {
    println("Validating: "+generate(ele))
    val r = if ((ele \ "valid").valueAs[Boolean])
      validator.validateValue(ele \ "data", ele \ "schema") must beSuccess
    else
      validator.validateValue(ele \ "data", ele \ "schema") must beFailure
    if (r.isFailure) {
      r.mapMessage(_ + " source: "+generate(ele))
    } else r
  }
  def testSyntax(ele: JValue): Result = {
    validator.validateSyntax(ele \ "schema") must beSuccess
  }
}
