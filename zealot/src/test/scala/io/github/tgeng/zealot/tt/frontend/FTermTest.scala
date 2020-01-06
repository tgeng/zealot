package io.github.tgeng.zealot.tt.frontend

import scala.language.implicitConversions
import org.junit.Test
import io.github.tgeng.fluentassert._
import io.github.tgeng.zealot.tt.core.Binder
import io.github.tgeng.zealot.tt.core.Builder
import io.github.tgeng.zealot.tt.core.Context
import io.github.tgeng.zealot.tt.frontend.FBuilder

import FTerm._
import FReference._
import FValue._
import FRedux._

class FTermTest {

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

  @Test
  def `ToTerm() - basic translations` = {
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

    FRdx(FApp(g1, g2)) should haveDeBruijnTerm((!1)(!0))
    FRdx(FPrj1(g1)) should haveDeBruijnTerm(p1(!1))
    FRdx(FPrj2(g1)) should haveDeBruijnTerm(p2(!1))
  }

  @Test
  def `ToTerm() - more complex translations` = {
    given ctx: DeBruijnContext = DeBruijnContext()

    fComposeTy should haveDeBruijnTerm(composeTy)
  }

  @Test
  def `ToFTerm() - basic translation` = {
    given ctx: Context[Binder]()

    {
      import Builder.{given, _}
      set(0)
    } should haveFrontendTerm {
      import FBuilder.{given, _}
      set(0)
    }

    {
      import Builder.{given, _}
      lam("x", !0)
    } should haveFrontendTerm {
      import FBuilder.{given, _}
      \("x") =>: !"x"
    }

    {
      import Builder.{given, _}
      ("A", set(0)) ->: !0
    } should haveFrontendTerm {
      import FBuilder.{given, _}
      ("A", set(0)) ->: !"A"
    }

    {
      import Builder.{given, _}
      ("A", set(0)) x !0
    } should haveFrontendTerm {
      import FBuilder.{given, _}
      ("A", set(0)) x !"A"
    }

    {
      import Builder.{given, _}
      lam("x", (!0, !0))
    } should haveFrontendTerm {
      import FBuilder.{given, _}
      \("x") =>: (!"x", !"x")
    }

    {
      import Builder.{given, _}
      unit
    } should haveFrontendTerm {
      import FBuilder.{given, _}
      unit
    }

    {
      import Builder.{given, _}
      *
    } should haveFrontendTerm {
      import FBuilder.{given, _}
      *
    }

    {
      import Builder.{given, _}
      lam("x", (!0)(!0))
    } should haveFrontendTerm {
      import FBuilder.{given, _}
      \("x") =>: (!"x")(!"x")
    }

    {
      import Builder.{given, _}
      lam("x", p1(!0))
    } should haveFrontendTerm {
      import FBuilder.{given, _}
      \("x") =>: p1(!"x")
    }

    {
      import Builder.{given, _}
      lam("x", p2(!0))
    } should haveFrontendTerm {
      import FBuilder.{given, _}
      \("x") =>: p2(!"x")
    }
  }

  @Test
  def `Term - round trip translation` = {
    import Builder.{given, _}
    Iterable(
      lam(!0),
      lam(lam((!0)(!1))),
      composeTy,
    ) shouldAll remainTheSameTermAfterRoundTrip
  }

  @Test
  def `FTerm - round trip translation` = {
    import FBuilder.{given, _}
    Iterable(
      \("x") =>: !"x",
      \("x", "x") =>: (!"x")(!"x"),
      \("x", "y") =>: (!"x")(!"y"),
      \("x") =>: (!"x")(\("x") =>: !"x"),
      fComposeTy
    ) shouldAll remainTheSameFTermAfterRoundTrip
  }
}
