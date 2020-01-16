package io.github.tgeng.zealot.tt.frontend

import scala.language.implicitConversions
import org.junit.Test
import io.github.tgeng.zealot.parsec._
import io.github.tgeng.zealot.tt.frontend._
import io.github.tgeng.zealot.tt.frontend.FBuilder.{given, _}

class FTermParserTest {
  @Test
  def `parse basic terms` = testing(fTermParser) {
    "Set0" succeedsWith set(0)
    "Set2" succeedsWith set(2)
    "Unit" succeedsWith unit
    "()" succeedsWith star
    "(())" succeedsWith star
    "((()))" succeedsWith star
  }

  @Test
  def `parse compound terms` = testing(fTermParser) {
    "someReference" succeedsWith "someReference".ref
    "Unit -> Unit" succeedsWith unit ->: unit
    "Unit -> Unit & Unit" succeedsWith unit ->: (unit &: unit)
    "Unit & Unit -> Unit" succeedsWith (unit &: unit) ->: unit
    "(Unit, Unit)" succeedsWith ft((unit, unit))
    "(Unit, Unit, Unit)" succeedsWith ft((unit, (unit, unit)))
     """\a, b => (a, b)""" succeedsWith \("a", "b") =>: ("a".ref, "b".ref)
     """\f => \x => f x""" succeedsWith \("f", "x") =>: "f".ref("x".ref)
    "(A: Set0) -> A" succeedsWith ("A", set(0)) ->: "A".ref
    "(A: Set0) & A" succeedsWith ("A", set(0)) &: "A".ref
    """\A, B => (x : A) & B x""" succeedsWith (
      \("A", "B") =>: (("x", "A".ref) &: "B".ref("x".ref))
    )
    """\A, B => (x : A) -> B x""" succeedsWith (
      \("A", "B") =>: (("x", "A".ref) ->: "B".ref("x".ref))
    )
    "(A : Set0) -> (Eq : A -> A -> Set0) -> (x : A) & Eq x x" succeedsWith (
      ("A", set(0)) ->:
      ("Eq", ("A".ref ->: "A".ref ->: set(0))) ->:
      (("x", "A".ref) &: ("Eq".ref)("x".ref)("x".ref))
    )
  }
}
