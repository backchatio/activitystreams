package io.backchat.jsonschema

import com.codahale.jerkson.AST._
import com.fasterxml.jackson.databind.JsonNode
import java.net.URL
import java.io.{Reader, File, InputStream}
import Json._

object AsJValue {

  trait AsJValue[T] {
    def asJValue(t: T): JValue
  }

  implicit object JValueAsJValue extends AsJValue[JValue] {
    def asJValue(t: JValue) = t
  }

  implicit object StringAsJValue extends AsJValue[String] {
    def asJValue(t: String) = parse(t)
  }

  implicit object jsonNodeAsJValue extends AsJValue[JsonNode] {
    def asJValue(t: JsonNode) = parse(t)
  }

  implicit object InputstreamAsJValue extends AsJValue[InputStream] {
    def asJValue(t: InputStream) = parse(t)
  }

  implicit object URLAsJValue extends AsJValue[URL] {
    def asJValue(t: URL) = parse(t)
  }

  implicit object FileAsJValue extends AsJValue[File] {
    def asJValue(t: File) = parse(t)
  }

  implicit object ReaderAsJValue extends AsJValue[Reader] {
    def asJValue(t: Reader) = parse(t)
  }

  implicit object ByteArrayAsJValue extends AsJValue[Array[Byte]] {
    def asJValue(t: Array[Byte]) = parse(t)
  }

}
