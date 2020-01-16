package io.github.tgeng.zealot.tt.core

import scala.language.implicitConversions
import org.junit.Test
import io.github.tgeng.fluentassert._
import io.github.tgeng.zealot.tt.core._
import io.github.tgeng.zealot.tt.core.Builder.{given, _}

class TypeCheckTest {

  given ctx : TypeContext = TypeContext()

  @Test
  def `basic type check` = {
    star :< unit
    (star, star) :< (unit &: unit)
    (unit, star) :< (set(0) &: 0.ref)
    lam(star) :< (unit ->: unit)
    set(1) :< set(2)
    set(0) :< set(2)
    (unit ->: unit) :< set(0)
    (unit ->: unit) :< set(1)
    (unit &: unit) :< set(0)
    (set(1) ->: set(2)) :< set(3)
    (set(2) ->: set(1)) :< set(3)
    (set(1) &: set(2)) :< set(3)
    (set(2) &: set(1)) :< set(3)

    set(1) shouldNot checkWithType(set(0))
    set(1) shouldNot checkWithType(set(1))
    (star) shouldNot checkWithType(star)
    (set(1) ->: set(2)) shouldNot checkWithType(set(2))
    lam(star) shouldNot checkWithType(star ->: unit)
  }

  @Test
  def `basic type infer` = {
    (star) :> unit
    set(0) :> set(1)
    set(1) :> set(2)
    (unit ->: unit) :> set(0)
    (unit &: unit) :> set(0)
    (set(1) ->: set(2)) :> set(3)
    (set(2) ->: set(1)) :> set(3)
    (set(1) &: set(2)) :> set(3)
    (set(2) &: set(1)) :> set(3)
  }

  @Test
  def `more type check` = {
    lam(p1(0.ref)) :< ((set(0) &: 0.ref) ->: set(0))
    lam(p2(0.ref)) :< ((set(0) &: 0.ref) ->: p1(0.ref))

    given ctx: TypeContext = context(set(0), set(0), set(0))

    0.nref :> set(0)
    2.ref :> set(0)
    1.nref :> set(0)
    1.ref :> set(0)
    2.nref :> set(0)
    0.ref :> set(0)

    (0.nref ->: 1.nref) :> set(0)
    (2.ref ->: 2.ref) :> set(0)
    (0.nref &: 1.nref) :> set(0)
    (2.ref &: 2.ref) :> set(0)

    // 3 : 0
    ctx += 0.nref

    3.nref :> 0.nref
    0.ref :> 0.nref
    (3.nref, 3.nref) :< (0.nref &: 0.nref)
    (0.ref, 0.ref) :< (0.nref &: 0.nref)
    lam(3.nref) :< (0.nref ->: 0.nref)
    lam(1.ref) :< (0.nref ->: 0.nref)
    lam(3.nref) :< (1.nref ->: 0.nref)
    lam(1.ref) :< (1.nref ->: 0.nref)

    // 4 : 0 -> 1
    ctx += 0.nref ->: 1.nref

    4.nref(3.nref) :> 1.nref
    (0.ref)(1.ref) :> 1.nref

    // 5 : 0 &: 1
    ctx += 0.nref &: 1.nref

    p1(5.nref) :> 0.nref
    p1(0.ref) :> 0.nref
    p2(5.nref) :> 1.nref
    p2(0.ref) :> 1.nref
  }

  @Test
  def `more lambda type check` = {
    lam(0.ref) :< (set(0) ->: set(0))

    // standard id function
    lam(lam(0.ref)) :< (set(0) ->: 0.ref ->: 1.ref)

    // (simple) compose function
    //            A   B   C   f   g   x  => f    (g   x)
    val compose = lam(lam(lam(lam(lam(lam((2.ref)((1.ref)(0.ref))))))))
    val composeTy =
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
    compose :< composeTy
  }

  def (t1: Term) :< (t2: Term)(given ctx: TypeContext) = t1 should checkWithType(t2)
  def (t1: Term) :> (t2: Term)(given ctx: TypeContext) = t1 should haveInferredType(t2)
}
