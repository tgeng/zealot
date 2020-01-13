package io.github.tgeng.zealot.parsec

import scala.language.implicitConversions
import org.junit.Test
import io.github.tgeng.fluentassert._
import io.github.tgeng.zealot.parsec.{given, _}

class ParsecTest {
  @Test
  def `basic parsers` = {
    testing(pure(())) {
      "" succeedsWith(())
    }
    testing(pure((1))) {
      "" succeedsWith 1
    }
    testing(position) {
      "" succeedsWith 0
    }
    testing(empty) {
      "" succeedsWith(())
      "abc" succeedsWith(())
    }
    testing(any) {
      "abc" succeedsWith 'a'
      "" failsWithMessage "0: <any>"
    }
    testing(eof) {
      "" succeedsWith(())
      "abc" failsWithMessage "0: <eof>"
    }
    testing(skip) {
      "abc" succeedsWith(())
      "" failsWithMessage "0: <skip>"
    }

    testing(anyOf("abc")) {
      "ccc" succeedsWith 'c'
      "" failsWithMessage "0: <anyOf{a, b, c}>"
    }

    testing('c') {
      "c" succeedsWith 'c'
      "change" succeedsWith 'c'
      "abc" failsWithMessage "0: 'c'"
    }

    testing("abc") {
      "abc" succeedsWith "abc"
      "abcdef" succeedsWith "abc"
      "def" failsWithMessage """0: "abc""""
    }

    testing("[0-9]+".r) {
      "123" succeedsWith "123"
      "123abc" succeedsWith "123"
      "abc" failsWithMessage """0: /[0-9]+/"""
    }
  }

  @Test
  def `or operator` = {
    testing("a" | "b" | "c" | "d") {
      "a" succeedsWith "a"
      "b" succeedsWith "b"
      "x" failsWithMessage """
        0: "d"
        0: "a" | "b" | "c" | "d"
      """
    }
  }

  @Test
  def `commit operator and |` = {
    val oct = "0" >> ~"[0-7]+".r withName "oct"
    val hex = "0x" >> ~"[0-9a-f]+".r withName "hex"
    val octOrHex = hex | oct | ".*".r

    testing(octOrHex) {
      "0xaaf" succeedsWith "aaf"
      "0123" succeedsWith "123"
      "0abc" failsWithMessage """
        1: ~/[0-7]+/
        0: oct
        0: hex | oct | /.*/
      """
    }
  }

  @Test
  def `commit operator and *` = {
    val p = ":" >> ~"\\w+".r
    testing(p*) {
      ":abc" succeedsWith Seq("abc" )
      ":abc:def" succeedsWith Seq("abc", "def" )
      ":abc:?" failsWithMessage """
        5: ~/\w+/
        4: ":" >> ~/\w+/
        0: (":" >> ~/\w+/)*
      """
    }
  }

  @Test
  def `not operator` = {
    val alphabet = satisfy[Char](Character.isAlphabetic(_))
    val keyword = ("def" | "class" | "val") << not(~alphabet)

    testing(keyword) {
      "def" succeedsWith "def"
      "class" succeedsWith "class"
      "val" succeedsWith "val"
      "definition" failsWithMessage """
        3: not ~<satisfy>
        0: ("def" | "class" | "val") << not ~<satisfy>
      """
    }
  }

  @Test
  def `and operator` = {
    val alphabet = satisfy[Char](Character.isAlphabetic(_)) withName "alphabet"
    val digit = satisfy[Char](Character.isDigit(_)) withName "digit"
    val keyword = ("def" | "class" | "val") << not(~alphabet) withName "keyword"
    val identifier = "\\w+".r & not(keyword) & not(digit) // not starting with digit

    testing(identifier) {
      "abc" succeedsWith "abc"
      "definition" succeedsWith "definition"
      "def" failsWithMessage """
        0: not keyword
        0: /\w+/ & not keyword & not digit
      """
      "123abc" failsWithMessage """
        0: not digit
        0: /\w+/ & not keyword & not digit
      """
    }
  }

  @Test
  def `suffix operators` = {
    val abc = parser("abc")
    testing(abc*) {
      "abcabcabd" succeedsWith Seq("abc", "abc" )
      "def" succeedsWith Seq[String]()
    }
    testing(abc+) {
      "abcabcabd" succeedsWith Seq("abc", "abc" )
      "def" failsWithMessage """
        0: "abc"
        0: "abc"+
      """
    }
    testing(abc?) {
      "abcabcabd".succeedsWith[Char, Option[String]](Some("abc"))
      "def".succeedsWith[Char, Option[String]](None)
    }
  }

  @Test
  def `repeat operator` = {
    testing(3 * "abc") {
      "abcabcabc" succeedsWith Seq("abc", "abc", "abc" )
      "abcabcabcabc" succeedsWith Seq("abc", "abc", "abc" )
      "abcabc" failsWithMessage """
        6: "abc"
        0: 3 * "abc"
      """
    }
  }

  @Test
  def `sepBy operator` = {
    val word = "\\w+".r withName "word"
    testing(word sepBy1 ',') {
      "abc" succeedsWith Seq("abc" )
      "abc,def" succeedsWith Seq("abc", "def" )
      "~~" failsWithMessage """
        0: word
        0: word sepBy1 ','
      """
    }
    testing(word sepBy ',') {
      "abc" succeedsWith Seq("abc" )
      "abc,def" succeedsWith Seq("abc", "def" )
      "~~" succeedsWith Seq[String]()
    }
    testing(word.sepByN(3)(',')) {
      "ab,cd,ef" succeedsWith Seq("ab", "cd", "ef" )
      "ab,cd,ef,gh" succeedsWith Seq("ab", "cd", "ef" )
      "ab,cd" failsWithMessage """
        5: ','
        5: ',' >> word
        2: 2 * (',' >> word)
        0: word sepByN(3) ','
      """
    }
  }

  @Test
  def `prefix and suffix` = {
    val word = "\\w+".r withName "word"
    testing('(' >> word << ')') {
    "(abc)" succeedsWith "abc"
    "(abc)def" succeedsWith "abc"
    "()" failsWithMessage """
      1: word
      0: '(' >> word << ')'
    """
    "abc" failsWithMessage """
      0: '('
      0: '(' >> word << ')'
    """
    }
  }

  @Test
  def `apply operator` = {
    val spaces = parser(' ')*
    val number = ("[0-9]+".r << spaces).map(_.toInt) withName "number"
    testing(pure((a: Int, b: Int) => a + b, "Sum2")(number, number)) {
      "12 34" succeedsWith 46
      "12 ab" failsWithMessage """
        3: /[0-9]+/
        3: number
        0: Sum2(number, number)
      """
    }
    testing(pure((a: Int, b: Int, c: Int) => a + b + c, "Sum3")(number, number, number)) {
      "12 34 56" succeedsWith 102
      "12 34" failsWithMessage """
        5: /[0-9]+/
        5: number
        0: Sum3(number, number, number)
      """
    }
    testing(pure((a: Int, b: Int, c: Int, d: Int) => a + b + c + d, "Sum4")(number, number, number, number)) {
    "12 34 56 78" succeedsWith 180
    "12 34 56 " failsWithMessage """
      9: /[0-9]+/
      9: number
      0: Sum4(number, number, number, number)
    """
    }
  }

  @Test
  def `chainedLeftBy and chainedRightBy` = {
    val op = ("+" | "-").map(op => ((a: String, b: String) => "(" + a + op + b + ")"))
    testing(".".r chainedLeftBy op) {
      "a+b+c" succeedsWith "((a+b)+c)"
    }
    testing(".".r chainedRightBy op) {
      "a+b+c" succeedsWith "(a+(b+c))"
    }
  }

  val realNumber : Parser[Char, Double] = for {
    sign <- ('-'?).map(_.map(_ => -1).getOrElse(1))
    beforePoint <- "[0-9]+".r
    afterPoint <- (('.' >> ~"[0-9]+".r)?)
      .map(_.map(s => s.toInt / math.pow(10.0, s.size))
            .getOrElse(0.0))
  } yield sign * (beforePoint.toInt + afterPoint)

  @Test
  def `prepend append concat` = {
    val a = parser('a')
    val b = parser('b')
    val c = parser('c')

    testing(a + b) {
      "abc".succeedsWith[Char, IndexedSeq[Char]]("ab")
      "a" failsWithMessage """
         1: 'b'
         0: 'a' + 'b'
      """
    }
    val ab = parser(a + b)
    testing(c +: ab) {
      "cab".succeedsWith[Char, IndexedSeq[Char]]("cab")
      "ab" failsWithMessage """
        0: 'c'
        0: 'c' +: 'a' + 'b'
      """
    }
    testing(ab :+ c) {
      "abc".succeedsWith[Char, IndexedSeq[Char]]("abc")
      "abd" failsWithMessage """
        2: 'c'
        0: 'a' + 'b' :+ 'c'
      """
    }
    testing(ab ++ ab) {
      "abab".succeedsWith[Char, IndexedSeq[Char]]("abab")
      "abc" failsWithMessage """
        2: 'a'
        0: 'a' + 'b' ++ 'a' + 'b'
      """
    }
  }

  @Test
  def `parse real number` = testing(realNumber) {
    "2" succeedsWith 2.0
    "-50" succeedsWith -50.0
    "-50.25" succeedsWith -50.25
    "30.5" succeedsWith 30.5
    "123a" succeedsWith 123.0
    "abc" failsWithMessage  """
      0: /[0-9]+/
      0: '-'? /[0-9]+/ <?>
    """
    "4." failsWithMessage  """
      2: ~/[0-9]+/
      1: '.' >> ~/[0-9]+/
      1: ('.' >> ~/[0-9]+/)?
      0: '-'? /[0-9]+/ ('.' >> ~/[0-9]+/)?
    """
  }

  @Test
  def `calculator` = {
    val spaces = parser(' ')*
    val plus = parser('+').map(_ => (a: Double, b: Double) => a + b)
    val minus = parser('-').map(_ => (a: Double, b: Double) => a - b)
    val multiply = parser('*').map(_ => (a: Double, b: Double) => a * b) withName "*"
    val divide = parser('/').map(_ => (a: Double, b: Double) => a / b) withName "/"

    def sumExpr: Parser[Char, Double] =
      prodExpr.chainedLeftBy(spaces >> (plus | minus) << spaces) withName "sumExpr"

    def prodExpr: Parser[Char, Double] =
      term.chainedLeftBy(spaces >> (multiply | divide) << spaces) withName "prodExpr"

    def term: Parser[Char, Double] =
      realNumber |
      "(" >> spaces >> sumExpr << spaces << ")" withName "term"

    testing(sumExpr << eof) {
      "1" succeedsWith 1.0
      "1+2" succeedsWith 3.0
      "1 + 2" succeedsWith 3.0
      "1 - 2" succeedsWith -1.0
      "1 * 2" succeedsWith 2.0
      "1 / 2" succeedsWith 0.5
      "1 + 2 * 3" succeedsWith 7.0
      "1 * 2 + 3" succeedsWith 5.0
      "1 * 2 + 3" succeedsWith 5.0
      "2 * (3 + 4)" succeedsWith 14.0
      "2 * ( 3 + 4 )" succeedsWith 14.0
      "1 + 2 * 3 + (4 - 5) * 6" succeedsWith 1.0
    }
  }
}
