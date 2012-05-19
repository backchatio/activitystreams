package io.backchat

import java.nio.charset.Charset
import java.util.Locale
import com.fasterxml.jackson.databind.DeserializationFeature
import rl.UrlCodingUtils
import mojolly.inflector.InflectorImports

package object jsonschema extends InflectorImports {

  object Json extends com.codahale.jerkson.Json {
    mapper.enable(DeserializationFeature.USE_BIG_DECIMAL_FOR_FLOATS)
  }
  val Utf8 = Charset.forName("UTF-8")
  val ENGLISH = Locale.ENGLISH

  private[jsonschema] class JsonSchemaString(s: String) {
    def blankOption = if (isBlank) None else Some(s)
    def isBlank = s == null || s.trim.isEmpty
    def nonBlank = !isBlank
    def asCheckboxBool = s.toUpperCase(ENGLISH) match {
      case "ON" | "TRUE" | "OK" | "1" | "CHECKED" ⇒ true
      case _                                      ⇒ false
    }
    def urlEncode: String = { // Encoding comforming to RFC 3986
      UrlCodingUtils.urlEncode(s)
    }
    def formEncode: String = { // This gives the same output as java.net.URLEncoder
      UrlCodingUtils.urlEncode(s, spaceIsPlus = true)
    }
    def urlDecode: String = {
      UrlCodingUtils.urlDecode(s, plusIsSpace = false)
    }

    def formDecode: String = { // This gives the same output as java.net.URLDecoder
      UrlCodingUtils.urlDecode(s, plusIsSpace = true)
    }

    def %(params: Any*) = s.format(params:_*)
  }

  private[jsonschema] implicit def string2richer(s: String) = new JsonSchemaString(s)

}