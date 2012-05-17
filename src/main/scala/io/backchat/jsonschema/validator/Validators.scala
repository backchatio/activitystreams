package io.backchat
package jsonschema
package validator

import java.util.concurrent.ConcurrentHashMap
import collection.mutable.ConcurrentMap
import collection.JavaConverters.asScalaConcurrentMapConverter
import com.codahale.jerkson.AST._
import scalaz._
import Scalaz._

trait Validators {
  val validators: ConcurrentMap[String, SchemaValidator] = new ConcurrentHashMap[String, SchemaValidator]().asScala

  def register(schemaValidators: SchemaValidator*) {
    validators ++= schemaValidators.map(s => s.property -> s)
  }

  def unregister(schemaValidators: SchemaValidator*) {
    validators --= schemaValidators.map(_.property)
  }

//  def validateSyntax(schema: JObject): ValidationNEL[ValidationError, JValue] = {
//    (schema.fields map {
//      case JField(name, _) => validators.get(name).map(_.validateSyntax(schema)) getOrElse schema.success
//    }).traverse[({type l[a] = ValidationNEL[ValidationError, a]})#l, JValue](_.liftFailNel)
//  }

  register(
    new TypeValidator,
    new DivisibleByValidator,
    new MinLengthValidator,
    new DisallowValidator,
    new MinimumValidator,
    new MaximumValidator
  )
}
