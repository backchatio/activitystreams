package io.backchat.jsonschema

sealed trait Error {
  def message: String
}
case class ValidationError(message: String, field: String) extends Error