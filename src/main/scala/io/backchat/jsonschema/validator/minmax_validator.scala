package io.backchat.jsonschema
package validator

import scalaz._
import Scalaz._
import Json._

trait MinMaxValidator extends SchemaValidator {

  def failMsg(exclusive: Boolean): String

  def validateSyntax(value: JValue): ValidationNEL[ValidationError, JValue] = {
    (value \ property, value \ ("exclusive"+property.capitalize)) match {
      case (JUndefined, _: JBoolean) =>
        ValidationError(
          "The exclusive%s property can't occur without the %s property" % (property.capitalize, property),
          property).failNel
      case (_: JNumber , _:JBoolean  | JNull | JUndefined) => value.successNel
      case (_: JNumber, _) =>
        ValidationError("The value of exclusive%s must be a boolean." % property.capitalize, property).failNel
      case _ => ValidationError("The value of %s must be a number" % property, property).failNel
    }
  }

  def validateValue(fieldName: String, value: JValue, schema: JValue): ValidationNEL[ValidationError, JValue] = {
    val isExclusive = (schema \ ("exclusive"+property.capitalize)) match {
      case JBoolean(r) => r
      case _ => false
    }

    (value \ fieldName, schema \ property) match {
      case (JInt(v), JInt(m)) if isValid(v, m, isExclusive) => value.successNel
      case (JInt(v), JDecimal(m)) if isValid(BigDecimal(v), m, isExclusive) => value.successNel
      case (JDecimal(v), JInt(m)) if isValid(v, BigDecimal(m), isExclusive) => value.successNel
      case (JDecimal(v), JDecimal(m)) if isValid(v, m, isExclusive) => value.successNel
      case (_: JInt | _: JFloat, _) =>
        ValidationError("The value %s is %s %s" % (generate(value \ fieldName), failMsg(isExclusive), generate(schema \ property)), fieldName).failNel
      case _ => ValidationError("The `%s` property can only contain numbers." % property, fieldName).failNel
    }
  }
//
//  protected def propertyValue(value: JValue) = if (classOf[JInt] == value.getClass) {
//    value.valueAs[BigInt].doubleValue()
//  } else value.valueAs[Double]

  protected def isValid(value: BigInt, bound: BigInt, exclusive: Boolean): Boolean
  protected def isValid(value: BigDecimal, bound: BigDecimal, exclusive: Boolean): Boolean
}

class MinimumValidator extends MinMaxValidator {
  val property: String = "minimum"

  def failMsg(exclusive: Boolean): String = if (exclusive) "less than" else "less than or equal to"

  protected def isValid(value: BigDecimal, bound: BigDecimal, exclusive: Boolean): Boolean = {
    if (exclusive) bound < value else bound <= value
  }

  protected def isValid(value: BigInt, bound: BigInt, exclusive: Boolean): Boolean = {
    if (exclusive) bound < value else bound <= value
  }
}

class MaximumValidator extends MinMaxValidator {
  val property: String = "maximum"

  def failMsg(exclusive: Boolean): String = if (exclusive) "larger than" else "larger than or equal to"

  protected def isValid(value: BigDecimal, bound: BigDecimal, exclusive: Boolean): Boolean = if (exclusive)
    bound > value
  else
    bound >= value

  protected def isValid(value: BigInt, bound: BigInt, exclusive: Boolean): Boolean = if (exclusive)
    bound > value
  else
    bound >= value
}

