package io.github.tgeng.zealot.parsec

import org.junit.Test
import io.github.tgeng.fluentassert._
import scala.language.implicitConversions
import io.github.tgeng.zealot.parsec._
import io.github.tgeng.zealot.parsec.Strings.given

class ParsecTest {
  @Test
  def `basic parsers` = {
    pure(()).parse("") should succeedWith(())
    val p1 : Parser[Char, Char] = 'c'
    (p1 | pure(())).parse("") should succeedWith(())

    // val p1 : Parser[Char, Char] = 'c'
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
  def `for comprehension` = {
    val realNumber : Parser[Char, Double] = for {
      sign <- ('-'?).map(_.map(_ => -1).getOrElse(1))
      beforePoint <- "[0-9]+".r
      afterPoint <- (('.' >> "[0-9]+".r)?).map(_.map(_.toInt).getOrElse(0))
    } yield sign * beforePoint.toInt + afterPoint

    realNumber.parse("2") should succeedWith(2.0)
  }
}
