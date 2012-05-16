package io.backchat.jsonschema

import java.net.URI

sealed trait Error {
  def message: String
}
case class ValidationError(message: String, field: String) extends Error




class SchemaNotFoundException(path: String) extends Exception("Couldn't find the schema at "+path)