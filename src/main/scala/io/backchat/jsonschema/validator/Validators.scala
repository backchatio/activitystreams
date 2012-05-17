package io.backchat
package jsonschema
package validator

import java.util.concurrent.ConcurrentHashMap
import collection.mutable.ConcurrentMap
import collection.JavaConverters.asScalaConcurrentMapConverter

trait Validators {
  val validators: ConcurrentMap[String, SchemaValidator] = new ConcurrentHashMap[String, SchemaValidator]().asScala

  def register(schemaValidators: SchemaValidator*) {
    validators ++= schemaValidators.map(s => s.property -> s)
  }

  def unregister(schemaValidators: SchemaValidator*) {
    validators --= schemaValidators.map(_.property)
  }

  register(
    new TypeValidator,
    new DivisibleByValidator,
    new MinLengthValidator,
    new DisallowValidator
  )
}
