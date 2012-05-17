//package io.backchat.jsonschema
//
//import com.codahale.jerkson.AST._
//import java.net.URI
//
//object PathResolver {
//
//  def apply(uri: URI)(implicit context: JsonSchema): PathHandler = {
//    if (uri.isAbsolute) {
//
//    }
//  }
//
//  trait PathHandler {
//    def schemes: Set[String]
//    def contentFor(uri: URI)(implicit context: JsonSchema): JValue
//  }
//
//  class HttpPathHandler extends PathHandler {
//    val scheme = Set("http", "https")
//  }
//
//  class FsPathHandler(implicit context: JsonSchema) extends PathHandler {
//    val scheme = Set("file")
//  }
//
//  class JValuePathHandler(implicit context: JsonSchema) extends PathHandler {
//    val scheme = Set("#")
//  }
//}
