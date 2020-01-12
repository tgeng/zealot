package io.github.tgeng.zealot.parsec

import scala.language.implicitConversions
import org.junit.Test
import io.github.tgeng.fluentassert._
import io.github.tgeng.zealot.parsec._
import io.github.tgeng.zealot.parsec.Strings.given

class ParsecTest {
  @Test
  def `basic parsers` = {
    pure(()).parse("") should succeedWith(())
    pure(1).parse("") should succeedWith(1)
    position.parse("") should succeedWith(0)
    empty.parse("") should succeedWith(())
    empty.parse("abc") should succeedWith(())
    any.parse("abc") should succeedWith('a')
    eof.parse("") should succeedWith(())
    eof.parse("abc") should failWithMessage("0: <eof>")
    skip.parse("abc") should succeedWith(())
    skip.parse("") should failWithMessage("0: <skip>")
    anyOf("abc").parse("ccc") should  succeedWith('c')
    anyOf("abc").parse("d") should  failWithMessage("0: <anyOf{a, b, c}>")

    val p1 : Parser[Char, Char] = 'c'
    p1.parse("c") should succeedWith('c')
    p1.parse("charge") should succeedWith('c')
    p1.parse("abc") should failWithMessage("0: 'c'")

    val p2 : Parser[Char, String] = "abc"
    p2.parse("abc") should succeedWith("abc")
    p2.parse("abcdef") should succeedWith("abc")
    p2.parse("def") should failWithMessage("""0: "abc"""")

    val p3 : Parser[Char, String] = "[0-9]+".r
    p3.parse("123") should succeedWith("123")
    p3.parse("123abc") should succeedWith("123")
    p3.parse("abc") should failWithMessage("""0: /[0-9]+/""")
  }

  @Test
  def `or operator` = {
    val abcd = "a" | "b" | "c" | "d"
    abcd.parse("a") should succeedWith("a")
    abcd.parse("b") should succeedWith("b")
    abcd.parse("x") should failWithMessage("""
      0: "d"
      0: "a" | "b" | "c" | "d"
    """)
  }

  @Test
  def `commit operator and |` = {
    val oct = "0" >> !"[0-7]+".r withName "oct"
    val hex = "0x" >> !"[0-9a-f]+".r withName "hex"
    val octOrHex = hex | oct | ".*".r

    octOrHex.parse("0xaaf") should succeedWith("aaf")
    octOrHex.parse("0123") should succeedWith("123")
    octOrHex.parse("0abc") should failWithMessage("""
      1: !/[0-7]+/
      0: oct
      0: hex | oct | /.*/
    """)
  }

  @Test
  def `commit operator and *` = {
    val p = ":" >> !"\\w+".r
    val ps = (p*)

    ps.parse(":abc") should succeedWith(Seq("abc"))
    ps.parse(":abc:def") should succeedWith(Seq("abc", "def"))
    ps.parse(":abc:?") should failWithMessage("""
      5: !/\w+/
      4: ":" >> !/\w+/
      0: (":" >> !/\w+/)*
    """)
  }

  @Test
  def `not operator` = {
    val alphabet = satisfy[Char](Character.isAlphabetic(_))
    val keyword = ("def" | "class" | "val") << not(!alphabet)

    keyword.parse("def") should succeedWith("def")
    keyword.parse("class") should succeedWith("class")
    keyword.parse("val") should succeedWith("val")
    keyword.parse("definition") should failWithMessage("""
      3: not !<satisfy>
      0: ("def" | "class" | "val") << not !<satisfy>
    """)
  }

  @Test
  def `and operator` = {
    val alphabet = satisfy[Char](Character.isAlphabetic(_)) withName "alphabet"
    val digit = satisfy[Char](Character.isDigit(_)) withName "digit"
    val keyword = ("def" | "class" | "val") << not(!alphabet) withName "keyword"
    val identifier = "\\w+".r & not(keyword) & not(digit) // not starting with digit

    identifier.parse("abc") should succeedWith("abc")
    identifier.parse("definition") should succeedWith("definition")
    identifier.parse("def") should failWithMessage("""
      0: not keyword
      0: /\w+/ & not keyword & not digit
    """)
    identifier.parse("123abc") should failWithMessage("""
      0: not digit
      0: /\w+/ & not keyword & not digit
    """)
  }

  @Test
  def `suffix operators` = {
    val abc = parser("abc")
    (abc*).parse("abcabcabd") should succeedWith(Seq("abc", "abc"))
    (abc*).parse("def") should succeedWith(Seq())
    (abc+).parse("abcabcabd") should succeedWith(Seq("abc", "abc"))
    (abc+).parse("def") should failWithMessage("""
      0: "abc"
      0: "abc"+
    """)
    (abc?).parse("abcabcabd") should succeedWith(Some("abc"))
    (abc?).parse("def") should succeedWith(None)
  }

  @Test
  def `repeat operator` = {
    val abc3 = 3 * "abc"
    abc3.parse("abcabcabc") should succeedWith(Seq("abc", "abc", "abc"))
    abc3.parse("abcabcabcabc") should succeedWith(Seq("abc", "abc", "abc"))
    abc3.parse("abcabc") should failWithMessage("""
      6: "abc"
      0: 3 * "abc"
    """)
  }

  @Test
  def `sepBy operator` = {
    val word = "\\w+".r withName "word"
    val oneOrMore = word sepBy1 ','
    oneOrMore.parse("abc") should succeedWith(Seq("abc"))
    oneOrMore.parse("abc,def") should succeedWith(Seq("abc", "def"))
    oneOrMore.parse("!!") should failWithMessage("""
      0: word
      0: word sepBy1 ','
    """)
    val many = word sepBy ','
    many.parse("abc") should succeedWith(Seq("abc"))
    many.parse("abc,def") should succeedWith(Seq("abc", "def"))
    many.parse("!!") should succeedWith(Seq())

    val three = word.sepByN(3)(',')
    three.parse("ab,cd,ef") should succeedWith(Seq("ab", "cd", "ef"))
    three.parse("ab,cd,ef,gh") should succeedWith(Seq("ab", "cd", "ef"))
    three.parse("ab,cd") should failWithMessage("""
      5: ','
      5: ',' >> word
      2: 2 * (',' >> word)
      0: word sepByN(3) ','
    """)
  }

  val realNumber : Parser[Char, Double] = for {
    sign <- ('-'?).map(_.map(_ => -1).getOrElse(1))
    beforePoint <- "[0-9]+".r
    afterPoint <- (('.' >> !"[0-9]+".r)?)
      .map(_.map(s => s.toInt / math.pow(10.0, s.size))
            .getOrElse(0.0))
  } yield sign * (beforePoint.toInt + afterPoint)

  @Test
  def `parse real number` = {
    realNumber.parse("2") should succeedWith(2.0)
    realNumber.parse("-50") should succeedWith(-50.0)
    realNumber.parse("-50.25") should succeedWith(-50.25)
    realNumber.parse("30.5") should succeedWith(30.5)
    realNumber.parse("123a") should succeedWith(123)
    realNumber.parse("abc") should failWithMessage( """
      0: /[0-9]+/
      0: '-'? /[0-9]+/ <?>
    """)
    realNumber.parse("4.") should failWithMessage( """
      2: !/[0-9]+/
      1: '.' >> !/[0-9]+/
      1: ('.' >> !/[0-9]+/)?
      0: '-'? /[0-9]+/ ('.' >> !/[0-9]+/)?
    """)
  }
}
