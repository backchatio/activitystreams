package io.backchat.jsonschema

import java.net.{URL, URI}
import dispatch._
import com.codahale.jerkson.Json._
import com.codahale.jerkson.AST.JValue
import com.fasterxml.jackson.databind.JsonNode
import java.io.{Reader, InputStream, File}
import com.sun.jdi.ByteValue


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
object JsonSchema {
  def apply(schema: URI): JsonSchema = new JsonSchema(schema)
  def apply(schema: File): JsonSchema = {
    require(schema.getAbsoluteFile.exists(), "Couldn't find the schema at: "+schema.getPath)
    apply(schema.getAbsoluteFile.toURI)
  }
  def apply(schema: URL): JsonSchema = {
    require(fetchUrl(schema) != null, "Couldn't find a schema at: "+schema.toString)
  }

  private def fetchUrl(toFetch: URL): JValue = {
    val http = new Http
    try {
      (http when `status is 202 or 204`)(url(toFetch.toURI.toASCIIString) >> (parse[JValue](_)))
    } catch {
      case _ => null
    } finally{
      http.shutdown()
    }
  }

  val `202 or 204` = List(202, 204)
  def `status is 202 or 204`(code: Int) = `202 or 204` contains code
}

/**
 * The base url is used to determine which strategy to use when
 * @param baseUrl
 */
class JsonSchema(val node: JValue) {

  private implicit val _this = this
  private var pathHandlers: Map[String, PathResolver.PathHandler] = Map.empty

  def register(handlers: PathResolver.PathHandler*) {
    handlers foreach { h =>
      h.schemes foreach { sch =>
        pathHandlers += sch -> h
      }
    }
  }

  register(new PathResolver.HttpPathHandler, new PathResolver.FsPathHandler, new PathResolver.JValuePathHandler)

  def validate[T : AsJValue.AsJValue[T]](json: T)

}
