package io.github.tgeng.zealot.parsec

import scala.language.implicitConversions
import scala.util.matching.Regex

private val regexKind = Kind(10, "regex")

given parserMatchingRegex : Conversion[Regex, Parser[Char, String]] = (r: Regex) => new Parser[Char, String] {
  override def kind : Kind = regexKind
  override def detailImpl = s"/$r/"
  override def parseImpl(input: ParserState[Char]) : Either[ParserError[Char] | Null, String] = {
    r.findPrefixOf(input.content.slice(input.position, input.content.length)) match {
      case Some(matched) => {
        input.position += matched.length
        Right(matched)
      }
      case None => Left(ParserError(input.position, this, null))
    }
  }
}

private val stringKind = Kind(10, "string")

given parserMatchingString : Conversion[String, Parser[Char, String]] = (s: String) => new Parser[Char, String] {
  override def kind : Kind = stringKind
  override def detailImpl = "\"" + s + "\""
  override def parseImpl(input: ParserState[Char]) : Either[ParserError[Char] | Null, String] = {
    if (input.content.slice(input.position, input.position + s.length) startsWith s) {
      input.position += s.length
      Right(s)
    } else {
      Left(ParserError(input.position, this, null))
    }
  }
}

given parserMatchingChar : Conversion[Char, Parser[Char, Char]] = (c: Char) => satisfy[Char](_ == c) withStrongName s"'$c'"
