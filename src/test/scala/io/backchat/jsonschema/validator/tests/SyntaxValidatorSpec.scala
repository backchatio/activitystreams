package io.backchat.jsonschema
package validator
package tests

import io.backchat.SpecificationBase
import Json._
import org.specs2.specification.Fragments

class SyntaxValidatorSpec extends SpecificationBase {

  def is = "When validating syntax" ^ syntaxEntries ^ end

  def syntaxEntries = {
    val JArray(entries) = parse[JArray](getClass.getResourceAsStream("/syntax/syntax.json"))
    entries map { e =>
      val valid = (e \ "valid").valueAs[Boolean]
      val schema = e \ "schema"
      "%s validation for %s".format(if (valid) "passes" else "fails", generate(schema)) ! {
        val validated = JsonSchema.validateSyntax(schema)
        if (valid)
          validated must beSuccess
        else
          validated must beFailure
      } : Fragments
    } reduce (_ ^ _)
  }
}
