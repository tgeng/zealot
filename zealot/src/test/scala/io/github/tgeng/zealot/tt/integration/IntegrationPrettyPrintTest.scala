package io.github.tgeng.zealot.tt.frontend

import org.junit.Test
import org.junit.Assert._
import io.github.tgeng.zealot.common._
import io.github.tgeng.fluentassert._
import io.github.tgeng.zealot.tt.frontend._

class PrettyPrintTest {
  given spec: ToStringSpec = ToStringSpec()

  @Test
  def `basic cases` = {
    !"Unit"
    !"Star"
    !"Set0"
    !"Set1"
    !"(x : Unit) -> Set0"
    !"(x : Unit) -> Set0 -> Set1"
    !"Unit -> Set0"
    !"\\x => y"
    !"\\x, y => z"
    !"(x : Unit) & Set0 & Set1"
    !"Unit & Set0"
    !"(Unit, Set0)"
    !"(Unit, Set0, Set1)"
    !"fn arg1 arg2"
    !"pair.1"
    !"pair.2"
    !"pair.1.2"
  }

  @Test
  def `compound cases` = {
    !"A & B -> C"
    !"(A -> B) & C"
    !"\\x => A & B"
    !"\\x => A -> B"
    !"\\x => (y, z)"
    !"\\x => a b c d e"
    !"(A -> B, A & B)"
  }

  private def (s: String)unary_!(given spec: ToStringSpec) = {
    val termString = s.trimIndent
    fTermParser.parse(termString) match {
      case Right(ft) if ft.toString(spec) == termString => ()
      case Right(ft) => ft.toString(spec) should equal(termString)
      case Left(e) => fail(e.toString)
    }
  }
}
