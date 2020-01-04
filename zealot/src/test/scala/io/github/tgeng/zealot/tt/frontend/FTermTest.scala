package io.github.tgeng.zealot.tt.frontend

import scala.language.implicitConversions
import org.junit.Test
import io.github.tgeng.fluentassert._
import io.github.tgeng.zealot.tt.core.Builder
import io.github.tgeng.zealot.tt.frontend.FBuilder

import FTerm._
import FReference._
import FValue._
import FRedux._

class FTermTest {
  @Test
  def `basic translations` = {
    import Builder.{given, _}

    given ctx: DeBruijnContext = DeBruijnContext()
    ctx += ("g1", "g2")

    val g1 = FRef(FName("g1"))
    g1 should haveDeBruijnTerm(!1)
    val g2 = FRef(FName("g2"))
    g2 should haveDeBruijnTerm(!0)

    val x = FRef(FName("x"))

    FVal(FSet(1)) should haveDeBruijnTerm(set(1))
    FVal(FPi("", g1, g2)) should haveDeBruijnTerm(!1 ->: !1)
    FVal(FPi("x", g1, x)) should haveDeBruijnTerm(!1 ->: !0)
    FVal(FLam("x", g1)) should haveDeBruijnTerm(lam(!2))
    FVal(FLam("x", x)) should haveDeBruijnTerm(lam(!0))
    FVal(FSig("", g1, g2)) should haveDeBruijnTerm(!1 x !1)
    FVal(FSig("x", g1, x)) should haveDeBruijnTerm(!1 x !0)
    FVal(FPair(g1, g2)) should haveDeBruijnTerm((!1, !0))
    FVal(FUnit()) should haveDeBruijnTerm(unit)
    FVal(FStar()) should haveDeBruijnTerm(*)
  }

  @Test
  def `more complex translations` = {
    given ctx: DeBruijnContext = DeBruijnContext()

    val fComposeTy = {
      import FBuilder.{given, _}
      ("A", set(0)) ->:
      ("B", set(0)) ->:
      ("C", !"B" ->: set(0)) ->:
      ("f", ("x", !"B") ->: (!"C")(!"x")) ->:
      ("g", !"A" ->: !"B") ->:
      ("x", !"A") ->:
      (!"C")((!"g")(!"x"))
    }

    val composeTy = {
      import Builder.{given, _}
      //A
        set(0) ->:
      //B
        set(0) ->:
      //C: B -> Set
        (!0 ->: set(0)) ->:
      //f:(x:B -> C x)
        (!1 ->: (!1)(!0)) ->:
      //g:A -> B
        (!3 ->: !3) ->:
      //x:A
        !4 ->:
      //C(g x)
        (!3)((!1)(!0))
    }

    fComposeTy should haveDeBruijnTerm(composeTy)
  }

  def (arg: (String, FTerm)) -->: (bodyTy: FTerm) : FTerm =
    FVal(FPi(arg._1, arg._2, bodyTy))
}
