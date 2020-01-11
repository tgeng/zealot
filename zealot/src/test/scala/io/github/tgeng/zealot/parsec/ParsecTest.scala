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
  def `commit operator` = {
    val oct = "0" >> !"[0-7]+".r withName "oct"
    val hex = "0x" >> !"[0-9a-f]+".r withName "hex"
    val octOrHex = hex | oct | ".*".r
    val manyHex = (hex+)

    octOrHex.parse("0xaaf") should succeedWith("aaf")
    octOrHex.parse("0123") should succeedWith("123")
    octOrHex.parse("0abc") should failWithMessage("""
      1: !/[0-7]+/
      0: oct
      0: hex | oct | /.*/
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
