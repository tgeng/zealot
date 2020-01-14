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
    "Star" succeedsWith star
  }

  @Test
  def `parse compound type terms` = testing(fTermParser) {
    "Unit -> Unit" succeedsWith (unit ->: unit)
    "Unit -> Unit & Unit" succeedsWith (unit ->: (unit &: unit))
    "Unit & Unit -> Unit" succeedsWith ((unit &: unit) ->: unit)
    "(A: Set0) -> A" succeedsWith (("A", set(0)) ->: "A".ref)
    "(A: Set0) & A" succeedsWith (("A", set(0)) &: "A".ref)
    // "(A : Set0) -> (Eq : A -> A -> Set0) -> ((x : A) & Eq x x)" succeedsWith (
      // ("A", set(0)) ->:
      // ("Eq", ("A".ref ->: "A".ref ->: set(0))) ->:
      // (("x", "A".ref) &: ("Eq".ref)("x".ref)("x".ref))
    // )
  }
}
