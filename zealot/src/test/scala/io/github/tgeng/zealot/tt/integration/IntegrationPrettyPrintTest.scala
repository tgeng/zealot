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
    !"Unit -> Set0"
    !"(x : Unit) -> Set0"
    !"(x : Unit) -> Set0 -> Set1"
    !"((x : Unit) -> Set0) -> Set1"
    !"(A -> B) -> C"
    !"\\x => y"
    !"\\x, y => z"
    !"(x : Unit) & Set0 & Set1"
    !"((x : Unit) & Set0) & Set1"
    !"Unit & Set0"
    !"(Unit, Set0)"
    !"(Unit, Set0, Set1)"
    !"((a, b), c)"
    !"(A & B) & C"
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
    !"(A & (B -> C), \\x => (y, z), (x : A) -> B & C)"
  }

  @Test
  def `limit width` = {
    given spec: ToStringSpec = ToStringSpec(10)
    //--------
    !"""
    (
      A -> B,
      blahblahblah,
      A &
      B &
      C &
      D &
      E,
      \x =>
        x
        y
        z
        a
        b
        c,
      \x, y, z, a, b, c =>
        x y z)
    """
  }

  @Test
  def `compose function type` = {
    !"""
    (A : Set0) ->
    (B : Set0) ->
    (C : B -> Set0) ->
    ((x : B) -> C x) ->
    (g : A -> B) ->
    (x : A) ->
    C (g x)
    """
  }

  private def (s: String)unary_!(given spec: ToStringSpec) = {
    val expected = s.trimIndent
    fTermParser.parse(expected) match {
      case Right(ft) => ft.toString(spec) should equal(expected)
      case Left(e) => fail(e.toString)
    }
  }
}
