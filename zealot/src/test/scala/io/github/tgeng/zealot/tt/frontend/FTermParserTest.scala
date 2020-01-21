package io.github.tgeng.zealot.tt.frontend

import scala.language.implicitConversions
import org.junit.Test
import io.github.tgeng.zealot.parsec._
import io.github.tgeng.zealot.tt.core.root
import io.github.tgeng.zealot.tt.frontend._
import io.github.tgeng.zealot.tt.frontend.FBuilder.{given, _}

class FTermParserTest {
  @Test
  def `parse basic terms` = testing(fTermParser) {
    ".zealot.Set" succeedsWith ft(root/"zealot"/"Set")
    "()" succeedsWith star
    "(())" succeedsWith star
    "((()))" succeedsWith star
    "pair.1" succeedsWith p1("pair".ref)
    "(pair.1)" succeedsWith p1("pair".ref)
    "pair.1.2" succeedsWith p2(p1("pair".ref))
    "(pair.1).2" succeedsWith p2(p1("pair".ref))
    "((pair.1).2)" succeedsWith p2(p1("pair".ref))
    ".Nat" succeedsWith ft(root/"Nat")
    ".foo.bar.Quux" succeedsWith ft(root/"foo"/"bar"/"Quux")
  }

  @Test
  def `parse compound terms` = testing(fTermParser) {
    val unit = ft(root/"zealot"/"Unit")
    val set = ft(root/"zealot"/"Set")
    "someReference" succeedsWith "someReference".ref
    ".zealot.Unit -> .zealot.Unit" succeedsWith unit ->: unit
    ".zealot.Unit -> .zealot.Unit & .zealot.Unit" succeedsWith unit ->: (unit &: unit)
    ".zealot.Unit & .zealot.Unit -> .zealot.Unit" succeedsWith (unit &: unit) ->: unit
    "(.zealot.Unit, .zealot.Unit)" succeedsWith ft((unit, unit))
    "(.zealot.Unit, .zealot.Unit, .zealot.Unit)" succeedsWith ft((unit, (unit, unit)))
     """\a, b => (a, b)""" succeedsWith \("a", "b") =>: ("a".ref, "b".ref)
     """\f => \x => f x""" succeedsWith \("f", "x") =>: "f".ref("x".ref)
    "(A: .zealot.Set) -> A" succeedsWith ("A", set) ->: "A".ref
    "(A: .zealot.Set) & A" succeedsWith ("A", set) &: "A".ref
    """\A, B => (x : A) & B x""" succeedsWith (
      \("A", "B") =>: (("x", "A".ref) &: "B".ref("x".ref))
    )
    """\A, B => (x : A) -> B x""" succeedsWith (
      \("A", "B") =>: (("x", "A".ref) ->: "B".ref("x".ref))
    )
    "(A : .zealot.Set) -> (Eq : A -> A -> .zealot.Set) -> (x : A) & Eq x x" succeedsWith (
      ("A", set) ->:
      ("Eq", ("A".ref ->: "A".ref ->: set)) ->:
      (("x", "A".ref) &: ("Eq".ref)("x".ref)("x".ref))
    )
    ".Nat -> .Nat -> .Nat" succeedsWith ft(root/"Nat") ->: ft(root/"Nat") ->: ft(root/"Nat")
    ".foo.bar.somePair.1" succeedsWith p1(root/"foo"/"bar"/"somePair")
  }
}
