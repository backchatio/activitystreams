package io.backchat.jsonschema
package validator
package formats

import com.codahale.jerkson.AST._
import org.joda.time.format.DateTimeFormat
import Json._
import scalaz._
import Scalaz._

trait DateStringValidating { self: Format =>
  def dateFormat: DateTimeFormat
}

abstract class DateStringValidator(formatString: String, val key: String) extends Format {

  lazy val dateFormat = DateTimeFormat.forPattern(formatString)

  import util.control.Exception._
  def apply(v1: JValue): Validation[ValidationError, JValue] =
    (catching(classOf[IllegalArgumentException])
      withApply(_ => ValidationError("Invalid %s %s for %s" % (key, generate(v1), formatString), "").fail)) {
      dateFormat.parseDateTime(v1.valueAs[String])
      v1.success
    }

  val types: Set[Class[_ <: JValue]] = Set(classOf[JString])
}

class DateFormatValidator extends DateStringValidator("yyyy-MM-dd", "date")

class DateTimeFormatValidator extends DateStringValidator("yyyy-MM-dd'T'HH:mm:ssZ", "date-time")

class TimeFormatValidator extends DateStringValidator("HH:mm:ss", "time")

class UtcMsecFormatValidator extends Format {
  val key: String = "utc-millisec"
  def apply(v1: JValue): Validation[ValidationError, JValue] = v1 match {
    case JInt(v) => validate(v).map(_ => v1)
    case JDecimal(v) => validate(v.toBigInt()).map(_ => v1)
    case JFloat(v) => validate(BigInt(v.toLong)).map(_ => v1)
    case _ => ValidationError("Only positive numeric values are allowed", "").fail
  }

  private def validate(v: BigInt) = {
    if (v.signum == -1) ValidationError("Epoch must be a positive number", "").fail
    else if (v / 1000 > 31) ValidationError("Epoch time would overflow", "").fail
    else v.success
  }

  val types: Set[Class[_ <: JValue]] = Set(classOf[JNumber])
}

