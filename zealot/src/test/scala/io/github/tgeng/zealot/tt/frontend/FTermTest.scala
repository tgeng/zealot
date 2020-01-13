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
    ("C", "B".ref ->: set(0)) ->:
    ("f", ("x", "B".ref) ->: ("C".ref)("x".ref)) ->:
    ("g", "A".ref ->: "B".ref) ->:
    ("x", "A".ref) ->:
    ("C".ref)(("g".ref)("x".ref))
  }

  val composeTy = {
    import Builder.{given, _}
    //A
      set(0) ->:
    //B
      set(0) ->:
    //C: B -> Set
      (0.ref ->: set(0)) ->:
    //f:(x:B -> C x)
      (1.ref ->: (1.ref)(0.ref)) ->:
    //g:A -> B
      (3.ref ->: 3.ref) ->:
    //x:A
      4.ref ->:
    //C(g x)
      (3.ref)((1.ref)(0.ref))
  }

  @Test
  def `ToTerm() - basic translations` = {
    import Builder.{given, _}

    given ctx: DeBruijnContext = DeBruijnContext()
    ctx += ("g1", "g2")

    val g1 = FRef(FName("g1"))
    g1 should haveDeBruijnTerm(1.ref)
    val g2 = FRef(FName("g2"))
    g2 should haveDeBruijnTerm(0.ref)

    val x = FRef(FName("x"))

    FVal(FSet(1)) should haveDeBruijnTerm(set(1))
    FVal(FPi("", g1, g2)) should haveDeBruijnTerm(1.ref ->: 1.ref)
    FVal(FPi("x", g1, x)) should haveDeBruijnTerm(1.ref ->: 0.ref)
    FVal(FLam("x", g1)) should haveDeBruijnTerm(lam(2.ref))
    FVal(FLam("x", x)) should haveDeBruijnTerm(lam(0.ref))
    FVal(FSig("", g1, g2)) should haveDeBruijnTerm(1.ref &: 1.ref)
    FVal(FSig("x", g1, x)) should haveDeBruijnTerm(1.ref &: 0.ref)
    FVal(FPair(g1, g2)) should haveDeBruijnTerm((1.ref, 0.ref))
    FVal(FUnit()) should haveDeBruijnTerm(unit)
    FVal(FStar()) should haveDeBruijnTerm(star)

    FRdx(FApp(g1, g2)) should haveDeBruijnTerm((1.ref)(0.ref))
    FRdx(FPrj1(g1)) should haveDeBruijnTerm(p1(1.ref))
    FRdx(FPrj2(g1)) should haveDeBruijnTerm(p2(1.ref))
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
      lam("x", 0.ref)
    } should haveFrontendTerm {
      import FBuilder.{given, _}
      \("x") =>: "x".ref
    }

    {
      import Builder.{given, _}
      ("A", set(0)) ->: 0.ref
    } should haveFrontendTerm {
      import FBuilder.{given, _}
      ("A", set(0)) ->: "A".ref
    }

    {
      import Builder.{given, _}
      ("A", set(0)) &: 0.ref
    } should haveFrontendTerm {
      import FBuilder.{given, _}
      ("A", set(0)) &: "A".ref
    }

    {
      import Builder.{given, _}
      lam("x", (0.ref, 0.ref))
    } should haveFrontendTerm {
      import FBuilder.{given, _}
      \("x") =>: ("x".ref, "x".ref)
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
      star
    } should haveFrontendTerm {
      import FBuilder.{given, _}
      star
    }

    {
      import Builder.{given, _}
      lam("x", (0.ref)(0.ref))
    } should haveFrontendTerm {
      import FBuilder.{given, _}
      \("x") =>: ("x".ref)("x".ref)
    }

    {
      import Builder.{given, _}
      lam("x", p1(0.ref))
    } should haveFrontendTerm {
      import FBuilder.{given, _}
      \("x") =>: p1("x".ref)
    }

    {
      import Builder.{given, _}
      lam("x", p2(0.ref))
    } should haveFrontendTerm {
      import FBuilder.{given, _}
      \("x") =>: p2("x".ref)
    }
  }

  @Test
  def `Term - round trip translation` = {
    import Builder.{given, _}
    Iterable(
      lam(0.ref),
      lam(lam((0.ref)(1.ref))),
      lam(star),
      lam((lam(1.ref) &: lam(0.ref))),
      lam((lam(1.ref) &: lam(1.ref))),
      lam((lam(1.ref) &: lam(2.ref))),
      composeTy,
    ) shouldAll remainTheSameTermAfterRoundTrip
  }

  @Test
  def `FTerm - round trip translation` = {
    import FBuilder.{given, _}
    Iterable(
      \("x") =>: "x".ref,
      \("x", "x") =>: ("x".ref)("x".ref),
      \("x", "y") =>: ("x".ref)("y".ref),
      \("x") =>: ("x".ref)(\("x") =>: "x".ref),
      \("") =>: star,
      fComposeTy,
    ) shouldAll remainTheSameFTermAfterRoundTrip
  }
}
