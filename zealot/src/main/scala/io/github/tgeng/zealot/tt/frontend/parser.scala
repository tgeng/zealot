package io.github.tgeng.zealot.tt.frontend

import scala.language.implicitConversions
import io.github.tgeng.zealot.tt.core.root
import io.github.tgeng.zealot.parsec.{given, _}
import io.github.tgeng.zealot.tt.frontend.FBuilder.{given, _}

type FTermParser = Parser[Char, FTerm]

private val spaces = "\\s*".r withName "<spaces>"

private val alphabet: Parser[Char, Char] =
  satisfy((c: Char) => Character.isAlphabetic(c)) withName "<alphabet>"
private val digit: Parser[Char, Char] =
  satisfy((c: Char) => Character.isDigit(c)) withName "<digit>"
private val alphanum: Parser[Char, Char] =
  satisfy((c: Char) => Character.isAlphabetic(c) || Character.isDigit(c)) withName "<alphanum>"

private val number = parser("[0-9]+".r).map(_.toInt) withName "<number>"

private val identifier = parser("[a-zA-Z]\\w*".r) withName "<identifier>"

private val reference : FTermParser = identifier.map(_.ref) withName "FRef"
private val globalReference : FTermParser =
  (('.' >> identifier)+).map(parts => ft(parts.foldLeft(root)((parent, name) => parent / name))) withName "FGlobal"

val tuple : FTermParser = (fTermParser sepBy (spaces >> ',' << spaces))
  .map(elems =>
    if (elems.isEmpty) {
      star
    } else {
      elems.reduceRight(tupleToPair(_, _))
    }) withName "<tuple>"

val singleton : FTermParser = (for {
  s <- (reference | globalReference |
        '(' >> spaces >> tuple << spaces << ')')
  p <- (('.'!) >>
        (parser('1').map[Char, Char, FTerm => FTerm](_ => p1) |
         parser('2').map[Char, Char, FTerm => FTerm](_ => p2))
       )*
} yield p.foldLeft(s)((t, p) => p(t))) withName "<singleton>"

val lambda : FTermParser = (for {
  _ <- ('\\'!)
  args <- identifier sepBy1 (spaces >> (','!) << spaces)
  _ <- spaces >> ("=>"!) << spaces
  body <- fTermParser
} yield args.foldRight(body)((arg: String, body: FTerm) => FTerm.FVal(FValue.FLam(arg, body)))) withName "<lambda>"

val application : FTermParser =
  ((lambda | singleton) sepBy1 spaces).map(_.reduceLeft(_(_))) withName "<application>"

private def typeDecl(subParser: FTermParser) : Parser[Char, (String, FTerm)] =
  '(' >> spaces >> (for {
    id <- identifier
    _ <- spaces >> ':' << spaces
    ty <- scoped{fTermParser}
  } yield (id, ty)) << spaces << ')' |
   scoped{subParser}.map(t => ("", t)) withName s"<typeDecl(${subParser.name()})>"

private val sigAmp : Parser[Char, ((String, FTerm), FTerm) => FTerm] =
  (spaces >> ("&"!) << spaces).map(_ => _ &: _)

private val product : FTermParser =
  foldRight(typeDecl(application), sigAmp, application) withName "<product>"

private val piArrow : Parser[Char, ((String, FTerm), FTerm) => FTerm] =
  (spaces >> ("->"!) << spaces).map(_ => _ ->: _)

val fTermParser: FTermParser =
  spaces >> foldRight(typeDecl(product), piArrow, product) withName "FTerm"
