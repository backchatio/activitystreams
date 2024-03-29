package io.backchat

/*
 * Copyright (c) 2011 Untyped Ltd, http://untyped.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

import java.awt.Color
import scala.util.parsing.combinator._

/**
 * Simple parser for CSS color literals. Usage:
 *
 * CssColorParser$.parseColor("#f00")
 *
 * Understands the following color formats:
 *
 * short hex strings       e.g.  #f00
 * long hex strings        e.g.  #ff0000
 * rgb function literals   e.g.  rgb(100, 0, 0)
 * rgba function literals  e.g.  rgba(100, 0, 0, 0.5)
 * hsl function literals   e.g.  hsl(180, 120, 120)
 * hsla function literals  e.g.  hsla(180, 120, 120, 0.5)
 * friendly color names    e.g.  red
 * "transparent" keyword
 *
 * @see https://gist.github.com/1284342
 */
object CssColorParser extends RegexParsers {

  // IPC: Don't use awt.Color it messes with tests in sbt
  case class CssColor(red: Int, green: Int, blue: Int, alpha: Int = 255)

  private[this] def parseHex(hex: String): Float = 1.0f * Integer.parseInt(hex, 16) / 255

   private[this] def hslColor(h: Float, s: Float, l: Float, a: Float): CssColor = {
     val base = Color.getHSBColor(h, s, l)
     if (a == 1.0f) CssColor(base.getRed, base.getGreen, base.getBlue)
     else new CssColor(base.getRed, base.getGreen, base.getBlue, asInt(a))
   }

   private[this] def parseInt(str: String, base: Int, min: Int, max: Int): Int =
     trimNumber(java.lang.Integer.parseInt(str, base), min, max)

   private[this] def trimNumber(num: Int, min: Int, max: Int) = math.min(math.max(num, min), max)

   private[this] def trimNumber(num: Float, min: Float, max: Float) = math.min(math.max(num, min), max)

   private[this]  def parseDegrees(str: String): Int = {
     val ans = java.lang.Integer.parseInt(str) % 360
     if (ans < 0) ans + 360 else ans
   }

   private[this] def parseFloat(str: String, min: Float, max: Float): Float =
     math.min(math.max(java.lang.Float.parseFloat(str), min), max)

  //TODO: Put this in parboiled if it turns out to be too slow

  override def skipWhitespace = true

  /**Entry point to the parser. */
  def parseColor(input: String): Option[CssColor] =
    parseAll(color, input.toLowerCase).map(Some(_)).getOrElse(None)

  private[this] val  doubleHex: Parser[Float] = "[0-9a-f]{2}".r ^^ (x => parseHex(x))

  private[this] val  singleHex: Parser[Float] = "[0-9a-f]".r ^^ (x => parseHex(x + x))

  private[this] lazy val named: Parser[CssColor] = "[a-z]+".r ^? namedColors

  private[this] val  longHex: Parser[CssColor] =
    ("#" ~> doubleHex ~ doubleHex ~ doubleHex) ^^ {
      case r ~ g ~ b => new CssColor(r.toInt, g.toInt, b.toInt)
    }

  private[this] val  shortHex: Parser[CssColor] =
    ("#" ~> singleHex ~ singleHex ~ singleHex) ^^ {
      case r ~ g ~ b => new CssColor(r.toInt, g.toInt, b.toInt)
    }

  private[this] val transparent: Parser[CssColor] =
    "transparent" ^^ (x => new CssColor(0, 0, 0, 0))

  private[this] val alphaArg: Parser[Float] =
    "-?[0-9]*.?[0-9]+".r ^^ (x => parseFloat(x, 0.0f, 1.0f))

  private[this] val percentArg: Parser[Float] =
    (("-?[0-9]+".r ~ "%") ^^ {
      case x ~ "%" => 1.0f * parseInt(x, 10, 0, 100) / 100
    })

  private[this] val byteArg: Parser[Float] =
    ("-?[0-9]+".r ^^ (x => 1.0f * parseInt(x, 10, 0, 255) / 255))

  private[this] val angleArg: Parser[Float] =
    ("-?[0-9]+".r ^^ (x => 1.0f * parseDegrees(x) / 360))

  private def asInt(f: Float) = (f*255+0.5).toInt

  private[this] val func: Parser[CssColor] =
    (("rgb(" ~ rgbArg ~ "," ~ rgbArg ~ "," ~ rgbArg ~ ")") ^^ {
      case _ ~ r ~ _ ~ g ~ _ ~ b ~ _ => new CssColor(asInt(r), asInt(g), asInt(b))
    }) |
      (("rgba(" ~ rgbArg ~ "," ~ rgbArg ~ "," ~ rgbArg ~ "," ~ alphaArg ~ ")") ^^ {
        case _ ~ r ~ _ ~ g ~ _ ~ b ~ _ ~ a ~ _ => new CssColor(asInt(r), asInt(g), asInt(b), asInt(a))
      }) |
      (("hsl(" ~ angleArg ~ "," ~ percentArg ~ "," ~ percentArg ~ ")") ^^ {
        case _ ~ h ~ _ ~ s ~ _ ~ l ~ _ => hslColor(h, s, l, 255)
      }) |
      (("hsla(" ~ angleArg ~ "," ~ percentArg ~ "," ~ percentArg ~ "," ~ alphaArg ~ ")") ^^ {
        case _ ~ h ~ _ ~ s ~ _ ~ l ~ _ ~ a ~ _ => hslColor(h, s, l, a)
      })


  private[this] val rgbArg: Parser[Float] =
    percentArg |
      byteArg


  /**Top parser rule */
  private[this] lazy val color: Parser[CssColor] =
    longHex |
      shortHex |
      named |
      func |
      transparent

  lazy val namedColors: Map[String, CssColor] =
    Map("aliceblue" -> "#f0f8ff",
      "antiquewhite" -> "#faebd7",
      "aqua" -> "#00ffff",
      "aquamarine" -> "#7fffd4",
      "azure" -> "#f0ffff",
      "beige" -> "#f5f5dc",
      "bisque" -> "#ffe4c4",
      "black" -> "#000000",
      "blanchedalmond" -> "#ffebcd",
      "blue" -> "#0000ff",
      "blueviolet" -> "#8a2be2",
      "brown" -> "#a52a2a",
      "burlywood" -> "#deb887",
      "cadetblue" -> "#5f9ea0",
      "chartreuse" -> "#7fff00",
      "chocolate" -> "#d2691e",
      "coral" -> "#ff7f50",
      "cornflowerblue" -> "#6495ed",
      "cornsilk" -> "#fff8dc",
      "crimson" -> "#dc143c",
      "cyan" -> "#00ffff",
      "darkblue" -> "#00008b",
      "darkcyan" -> "#008b8b",
      "darkgoldenrod" -> "#b8860b",
      "darkgray" -> "#a9a9a9",
      "darkgrey" -> "#a9a9a9",
      "darkgreen" -> "#006400",
      "darkkhaki" -> "#bdb76b",
      "darkmagenta" -> "#8b008b",
      "darkolivegreen" -> "#556b2f",
      "darkorange" -> "#ff8c00",
      "darkorchid" -> "#9932cc",
      "darkred" -> "#8b0000",
      "darksalmon" -> "#e9967a",
      "darkseagreen" -> "#8fbc8f",
      "darkslateblue" -> "#483d8b",
      "darkslategray" -> "#2f4f4f",
      "darkslategrey" -> "#2f4f4f",
      "darkturquoise" -> "#00ced1",
      "darkviolet" -> "#9400d3",
      "deeppink" -> "#ff1493",
      "deepskyblue" -> "#00bfff",
      "dimgray" -> "#696969",
      "dimgrey" -> "#696969",
      "dodgerblue" -> "#1e90ff",
      "firebrick" -> "#b22222",
      "floralwhite" -> "#fffaf0",
      "forestgreen" -> "#228b22",
      "fuchsia" -> "#ff00ff",
      "gainsboro" -> "#dcdcdc",
      "ghostwhite" -> "#f8f8ff",
      "gold" -> "#ffd700",
      "goldenrod" -> "#daa520",
      "gray" -> "#808080",
      "grey" -> "#808080",
      "green" -> "#008000",
      "greenyellow" -> "#adff2f",
      "honeydew" -> "#f0fff0",
      "hotpink" -> "#ff69b4",
      "indianred" -> "#cd5c5c",
      "indigo" -> "#4b0082",
      "ivory" -> "#fffff0",
      "khaki" -> "#f0e68c",
      "lavender" -> "#e6e6fa",
      "lavenderblush" -> "#fff0f5",
      "lawngreen" -> "#7cfc00",
      "lemonchiffon" -> "#fffacd",
      "lightblue" -> "#add8e6",
      "lightcoral" -> "#f08080",
      "lightcyan" -> "#e0ffff",
      "lightgoldenrodyellow" -> "#fafad2",
      "lightgray" -> "#d3d3d3",
      "lightgrey" -> "#d3d3d3",
      "lightgreen" -> "#90ee90",
      "lightpink" -> "#ffb6c1",
      "lightsalmon" -> "#ffa07a",
      "lightseagreen" -> "#20b2aa",
      "lightskyblue" -> "#87cefa",
      "lightslategray" -> "#778899",
      "lightslategrey" -> "#778899",
      "lightsteelblue" -> "#b0c4de",
      "lightyellow" -> "#ffffe0",
      "lime" -> "#00ff00",
      "limegreen" -> "#32cd32",
      "linen" -> "#faf0e6",
      "magenta" -> "#ff00ff",
      "maroon" -> "#800000",
      "mediumaquamarine" -> "#66cdaa",
      "mediumblue" -> "#0000cd",
      "mediumorchid" -> "#ba55d3",
      "mediumpurple" -> "#9370d8",
      "mediumseagreen" -> "#3cb371",
      "mediumslateblue" -> "#7b68ee",
      "mediumspringgreen" -> "#00fa9a",
      "mediumturquoise" -> "#48d1cc",
      "mediumvioletred" -> "#c71585",
      "midnightblue" -> "#191970",
      "mintcream" -> "#f5fffa",
      "mistyrose" -> "#ffe4e1",
      "moccasin" -> "#ffe4b5",
      "navajowhite" -> "#ffdead",
      "navy" -> "#000080",
      "oldlace" -> "#fdf5e6",
      "olive" -> "#808000",
      "olivedrab" -> "#6b8e23",
      "orange" -> "#ffa500",
      "orangered" -> "#ff4500",
      "orchid" -> "#da70d6",
      "palegoldenrod" -> "#eee8aa",
      "palegreen" -> "#98fb98",
      "paleturquoise" -> "#afeeee",
      "palevioletred" -> "#d87093",
      "papayawhip" -> "#ffefd5",
      "peachpuff" -> "#ffdab9",
      "peru" -> "#cd853f",
      "pink" -> "#ffc0cb",
      "plum" -> "#dda0dd",
      "powderblue" -> "#b0e0e6",
      "purple" -> "#800080",
      "red" -> "#ff0000",
      "rosybrown" -> "#bc8f8f",
      "royalblue" -> "#4169e1",
      "saddlebrown" -> "#8b4513",
      "salmon" -> "#fa8072",
      "sandybrown" -> "#f4a460",
      "seagreen" -> "#2e8b57",
      "seashell" -> "#fff5ee",
      "sienna" -> "#a0522d",
      "silver" -> "#c0c0c0",
      "skyblue" -> "#87ceeb",
      "slateblue" -> "#6a5acd",
      "slategray" -> "#708090",
      "slategrey" -> "#708090",
      "snow" -> "#fffafa",
      "springgreen" -> "#00ff7f",
      "steelblue" -> "#4682b4",
      "tan" -> "#d2b48c",
      "teal" -> "#008080",
      "thistle" -> "#d8bfd8",
      "tomato" -> "#ff6347",
      "turquoise" -> "#40e0d0",
      "violet" -> "#ee82ee",
      "wheat" -> "#f5deb3",
      "white" -> "#ffffff",
      "whitesmoke" -> "#f5f5f5",
      "yellow" -> "#ffff00",
      "yellowgreen" -> "#9acd32").map {
      case (k, v) => k -> parseColor(v).get
    }



}