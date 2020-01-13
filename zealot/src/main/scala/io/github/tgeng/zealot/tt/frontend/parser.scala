package io.github.tgeng.zealot.tt.frontend

import scala.language.implicitConversions
import io.github.tgeng.zealot.parsec.{given, _}
import io.github.tgeng.zealot.tt.frontend.FBuilder.{given, _}

type FTermParser = Parser[Char, FTerm]

private val space = parser(' ')
private val spaces = space*

private val alphabet: Parser[Char, Char] =
  satisfy((c: Char) => Character.isAlphabetic(c)) withName "<alphabet>"
private val digit: Parser[Char, Char] =
  satisfy((c: Char) => Character.isDigit(c)) withName "<digit>"
private val alphanum: Parser[Char, Char] =
  satisfy((c: Char) => Character.isAlphabetic(c) || Character.isDigit(c)) withName "<alphanum>"

private val number = parser("[0-9]+".r).map(_.toInt) withName "<number>"

private val setP : FTermParser = ("Set" >> number << not(alphanum)).map(set) withName "FSet"
private val unitP : FTermParser = ("Unit" << not(alphanum)).map(_ => unit) withName "FUnit"
private val starP : FTermParser = ("Star" << not(alphanum)).map(_ => star) withName "FStar"

private val reserved = setP | unitP

private val identifier = parser("[a-zA-Z]\\w*".r) & not(reserved) withName "<identifier>"

private val reference : FTermParser = identifier.map(_.ref) withName "FRef"

private val singleton = setP | unitP | starP | reference | '(' >> spaces >> fTermParser << spaces << ')' withName "<singleton>"

private def typeDecl(subParser: FTermParser) : Parser[Char, (String, FTerm)] =
  '(' >> spaces >> (for {
    id <- identifier
    _ <- spaces >> ':' << spaces
    ty <- subParser
  } yield (id, ty)) << spaces << ')' |
   subParser.map(t => ("", t))

private def sigCross : Parser[Char, ((String, FTerm), FTerm) => FTerm] =
  (spaces >> ("&") << spaces).map(_ => _ &: _)

private def product : FTermParser = foldRight(typeDecl(singleton), sigCross, singleton) withName "<product>"

private def piArrow : Parser[Char, ((String, FTerm), FTerm) => FTerm] =
  (spaces >> ("->") << spaces).map(_ => _ ->: _)

def fTermParser: FTermParser =  foldRight(typeDecl(product), piArrow, product) withName "FTerm"
