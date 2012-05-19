package io.backchat.jsonschema
package validator

import scalaz._
import Scalaz._
import Json._

trait NumericBoundValidator extends SchemaValidator {

  def failMsg(exclusive: Boolean): String

  def validateSyntax(value: JValue): Validation[ValidationError, JValue] =  (value \ property) match {
    case _: JInt => value.success
    case _: JFloat | _: JDecimal => value.success
    case _ => ValidationError("The value of %s must be an integer" % property, property).fail
  }

  def validateValue(fieldName: String, value: JValue, schema: JValue): Validation[ValidationError, JValue] = {
    val isExclusive = (schema \ ("exclusive"+property.capitalize)) match {
      case JBoolean(r) => r
      case _ => false
    }

    (value \ fieldName, schema \ property) match {
      case (JInt(v), JInt(m)) if isValid(v, m, isExclusive) => value.success
      case (JInt(v), JDecimal(m)) if isValid(BigDecimal(v), m, isExclusive) => value.success
      case (JDecimal(v), JInt(m)) if isValid(v, BigDecimal(m), isExclusive) => value.success
      case (JDecimal(v), JDecimal(m)) if isValid(v, m, isExclusive) => value.success
      case (_: JInt | _: JFloat, _) =>
        ValidationError("The value %s is %s %s" % (generate(value \ fieldName), failMsg(isExclusive), generate(schema \ property)), fieldName).fail
      case _ => ValidationError("The `%s` property can only contain numbers." % property, fieldName).fail
    }
  }
//
//  protected def propertyValue(value: JValue) = if (classOf[JInt] == value.getClass) {
//    value.valueAs[BigInt].doubleValue()
//  } else value.valueAs[Double]

  protected def isValid(value: BigInt, bound: BigInt, exclusive: Boolean): Boolean
  protected def isValid(value: BigDecimal, bound: BigDecimal, exclusive: Boolean): Boolean
}

class MinimumValidator extends NumericBoundValidator {
  val property: String = "minimum"

  def failMsg(exclusive: Boolean): String = if (exclusive) "less than" else "less than or equal to"

  protected def isValid(value: BigDecimal, bound: BigDecimal, exclusive: Boolean): Boolean = {
    if (exclusive) bound < value else bound <= value
  }

  protected def isValid(value: BigInt, bound: BigInt, exclusive: Boolean): Boolean = {
    if (exclusive) bound < value else bound <= value
  }
}

class MaximumValidator extends NumericBoundValidator {
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

