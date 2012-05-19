package io.backchat.jsonschema

import java.net.URI

sealed trait Error {
  def message: String
}

object ValidationError {
  def apply(message: String): ValidationError = apply(message, None)
  def apply(message: String, field: String): ValidationError = apply(message, field.blankOption)
}
case class ValidationError(message: String, field: Option[String]) extends Error




class SchemaNotFoundException(path: String) extends Exception("Couldn't find the schema at "+path)