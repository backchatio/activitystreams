package io.backchat
package jsonschema
package validator

import formats._
import java.util.concurrent.ConcurrentHashMap
import collection.mutable.ConcurrentMap
import collection.JavaConverters.asScalaConcurrentMapConverter
import scalaz._
import Scalaz._
import Json._

trait Validators {
  private[validator] val validators: ConcurrentMap[String, SchemaValidator] = new ConcurrentHashMap[String, SchemaValidator]().asScala
  private[validator] val formats: ConcurrentMap[String, Format] = new ConcurrentHashMap[String, Format]().asScala

  def register(schemaValidators: SchemaValidator*) {
    validators ++= schemaValidators.map(s => s.property -> s)
  }

  def unregister(schemaValidators: SchemaValidator*) {
    validators --= schemaValidators.map(_.property)
  }

  def registerFormats(schemaFormats: Format*) {
    formats ++= schemaFormats.map(s => s.key -> s)
  }

  def unregisterFormats(schemaFormats: Format*) {
    formats --= schemaFormats.map(_.key)
  }

  def format(name: String) = formats.get(name)
  def validator(name: String) = validators.get(name)

  def validateSyntax(schemaValue: JValue): ValidationNEL[ValidationError, JValue] = schemaValue match {
    case JObject(Nil) => schemaValue.success
    case schema: JObject =>

      (schema.fields map {
        case JField(name, _) =>
          val nm = if (name.startsWith("exclusive")) name.substring("exclusive".length).camelize else name
          validators.get(nm).map(_.validateSyntax(schema)) getOrElse schema.success
      }).traverse[({type l[a] = ValidationNEL[ValidationError, a]})#l, JValue](_.liftFailNel) match {
        case Success(_) => schema.success
        case Failure(f) => f.fail
      }
    case _ => ValidationError("Only single object schema's are allowed", "schema").failNel
  }

  register(
    new TypeValidator,
    new DivisibleByValidator,
    new MinLengthValidator,
    new DisallowValidator,
    new MinimumValidator,
    new MaximumValidator,
    new MaxLengthValidator,
    new MaxItemsValidator,
    new MinItemsValidator,
    new UniqueItemsValidator,
    new EnumValidator,
    new PatternValidator,
    new PropertiesValidator,
    new PatternPropertiesValidator,
    new AdditionalPropertiesValidator,
    new ItemsValidator,
    new RequiredValidator,
    new AdditionalItemsValidator,
    new DependenciesValidator,
    new TitleValidator,
    new DescriptionValidator,
    new IdValidator,
    new JsonRefValidator,
    new BaseSchemaValidator,
    new ExtendsValidator
  )

  registerFormats(
    new CssColorFormatValidator,
    new CssStyleFormatValidator,
    new DateFormatValidator,
    new DateTimeFormatValidator,
    new TimeFormatValidator,
    new UtcMsecFormatValidator,
    new EmailFormatValidator,
    new HostnameFormatValidator,
    new Ipv4AddressFormatValidator,
    new Ipv6AddressFormatValidator,
    new PhoneNumberFormatValidator,
    new RegexFormatValidator,
    new URIFormatValidator
  )
}
