package io.backchat.jsonschema
package tests

import org.specs2.Specification

class EcmaRegexSpec extends Specification {

  def is =
    "An EcmaRegex should" ^
      "return valid for a valid regex" ! validRegex ^
      "return invalid for an invalid regex" ! invalidRegex ^
      "match a string" ! matchesString ^
      "not falsely match a string" ! doesNotMatchString ^
    end

  def validRegex = EcmaRegex.isValid("hello\\s*") must beTrue
  def invalidRegex = EcmaRegex.isValid("(?<={index:)\\d+(?=})") must beFalse
  def matchesString = EcmaRegex.matches("hello\\s*", "hello world") must beTrue
  def doesNotMatchString = EcmaRegex.matches("hello\\s*", "goodbye world") must beFalse
}
