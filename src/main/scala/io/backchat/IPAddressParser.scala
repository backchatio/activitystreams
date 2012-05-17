package io.backchat

import util.parsing.combinator._

object IPAddressParser extends RegexParsers {

  //TODO: Put this in parboiled if it turns out to be too slow

  override def skipWhitespace = false

  private[this] val subDelimChars = """[!$&'()*+,;=]""".r
  private[this] val genDelimChars = """[:/?#\[\]@]""".r
  private[this] val hexDigits = """[0123456789abcdefABCDEF]""".r

  def validIPv4Address(input: String): Boolean = parseAll(IPv4Address, input.toLowerCase).successful
  def validIPv6Address(input: String): Boolean = parseAll(IP6Address, input.toLowerCase).successful

  private def hexDigit = hexDigits
  private def decOctet = """25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d""".r
  private def dottedDecOctet = decOctet <~ "."

  private def IPv4Address = dottedDecOctet ~ dottedDecOctet ~ dottedDecOctet ~ decOctet ^^ {
    case a ~ b ~ c ~ d ⇒ a + "." + b + "." + c + "." + d
  }

  private def h16_2 = repN(2, hexDigit)
  private def h16_3 = repN(3, hexDigit)
  private def h16_4 = repN(4, hexDigit)
  private def h16_multi = (h16_4 | h16_3 | h16_2) ^^ { _ mkString "" }
  private def h16 = h16_multi | hexDigit

  private def h16Colon = h16 ~ ":" ^^ { case a ~ b ⇒ a + b }
  private def h16Colon_2 = h16Colon ~ h16Colon ^^ { case a ~ b ⇒ a + b }
  private def h16Colon_3 = repN(3, h16Colon) ^^ { _ mkString "" }
  private def h16Colon_4 = repN(4, h16Colon) ^^ { _ mkString "" }
  private def h16Colon_5 = repN(5, h16Colon) ^^ { _ mkString "" }
  private def h16Colon_6 = repN(6, h16Colon) ^^ { _ mkString "" }
  private def h16Wrap(parser: Parser[String]): Parser[String] = parser ~ h16 ^^ { case a ~ b ⇒ a + b }
  private def h16ColonN(max: Int) = max match {
    case 6 ⇒ h16Wrap(h16Colon_6) | h16Wrap(h16Colon_5) | h16Wrap(h16Colon_4) | h16Wrap(h16Colon_3) | h16Wrap(h16Colon_2) | h16Wrap(h16Colon)
    case 5 ⇒ h16Wrap(h16Colon_5) | h16Wrap(h16Colon_4) | h16Wrap(h16Colon_3) | h16Wrap(h16Colon_2) | h16Wrap(h16Colon)
    case 4 ⇒ h16Wrap(h16Colon_4) | h16Wrap(h16Colon_3) | h16Wrap(h16Colon_2) | h16Wrap(h16Colon)
    case 3 ⇒ h16Wrap(h16Colon_3) | h16Wrap(h16Colon_2) | h16Wrap(h16Colon)
    case 2 ⇒ h16Wrap(h16Colon_2) | h16Wrap(h16Colon)
    case 1 ⇒ h16Wrap(h16Colon)
  }
  private def h16Colonh16N(max: Int) = h16ColonN(max) | h16
  private def nH16Colon(n: Int) = repN(n, h16Colon) ^^ { _ mkString "" }

  private def flatOpt(parser: ⇒ Parser[String]): Parser[String] = opt(parser) ^^ { _ getOrElse "" }

  private def ls32 = (h16Colon ~ h16 ^^ { case a ~ b ⇒ a + b }) | IPv4Address

  private def ip6_1 = nH16Colon(6) ~ ls32 ^^ { case a ~ b ⇒ a + b }
  private def ip6_2 = "::" ~ nH16Colon(5) ~ ls32 ^^ { case a ~ b ~ c ⇒ a + b + c }
  private def ip6_3 = flatOpt(h16) ~ "::" ~ nH16Colon(4) ~ ls32 ^^ { case a ~ b ~ c ~ d ⇒ a + b + c + d }
  private def ip6_4 = flatOpt(h16Colonh16N(1)) ~ "::" ~ nH16Colon(3) ~ ls32 ^^ { case a ~ b ~ c ~ d ⇒ a + b + c + d }
  private def ip6_5 = flatOpt(h16Colonh16N(2)) ~ "::" ~ nH16Colon(2) ~ ls32 ^^ { case a ~ b ~ c ~ d ⇒ a + b + c + d }
  private def ip6_6 = flatOpt(h16Colonh16N(3)) ~ "::" ~ nH16Colon(1) ~ ls32 ^^ { case a ~ b ~ c ~ d ⇒ a + b + c + d }
  private def ip6_7 = flatOpt(h16Colonh16N(4)) ~ "::" ~ ls32 ^^ { case a ~ b ~ c ⇒ a + b + c }
  private def ip6_8 = flatOpt(h16Colonh16N(5)) ~ "::" ~ h16 ^^ { case a ~ b ~ c ⇒ a + b + c }
  private def ip6_9 = flatOpt(h16Colonh16N(6)) ~ "::" ^^ { case a ~ b ⇒ a + b }
  def IP6Address = ip6_1 | ip6_2 | ip6_3 | ip6_4 | ip6_5 | ip6_6 | ip6_7 | ip6_8 | ip6_9



}
