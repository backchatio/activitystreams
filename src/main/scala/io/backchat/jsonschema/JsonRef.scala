package io.backchat.jsonschema

import java.net.URI
import com.codahale.jerkson.AST._

class JsonRef(uri: URI) {

  def resolve: JValue = PathResolver(uri).json
}
