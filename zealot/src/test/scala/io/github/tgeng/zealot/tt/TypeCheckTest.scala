package io.github.tgeng.zealot.tt

import scala.language.implicitConversions
import org.junit.Test
import io.github.tgeng.fluentassert._
import io.github.tgeng.zealot.tt._
import io.github.tgeng.zealot.tt.Builder.{given, _}

class TypeCheckTest {

  given ctx : Context = Context()

  @Test
  def `basic type check` = {
    (*) :< unit
    (*, *) :< (unit x unit)
    (unit, *) :< (set(0) x !0)
    lam(*) :< (unit ->: unit)
    set(1) :< set(2)
    set(0) :< set(2)
    (unit ->: unit) :< set(0)
    (unit ->: unit) :< set(1)
    (unit x unit) :< set(0)
    (set(1) ->: set(2)) :< set(3)
    (set(2) ->: set(1)) :< set(3)
    (set(1) x set(2)) :< set(3)
    (set(2) x set(1)) :< set(3)

    set(1) shouldNot checkWithType(set(0))
    set(1) shouldNot checkWithType(set(1))
    (*) shouldNot checkWithType(*)
    (set(1) ->: set(2)) shouldNot checkWithType(set(2))
  }

  @Test
  def `basic type infer` = {
    (*) :> unit
    set(0) :> set(1)
    set(1) :> set(2)
    (unit ->: unit) :> set(0)
    (unit x unit) :> set(0)
    (set(1) ->: set(2)) :> set(3)
    (set(2) ->: set(1)) :> set(3)
    (set(1) x set(2)) :> set(3)
    (set(2) x set(1)) :> set(3)
  }

  @Test
  def `more type check` = {
    given ctx: Context = context(set(0), set(0), set(0))
    
    0.nref :> set(0)
    !2 :> set(0)
    1.nref :> set(0)
    !1 :> set(0)
    2.nref :> set(0)
    !0 :> set(0)

    (0.nref ->: 1.nref) :> set(0)
    (!2 ->: !2) :> set(0)
    (0.nref x 1.nref) :> set(0)
    (!2 x !2) :> set(0)

    // 3 : 0
    ctx.append(0.nref)
    
    3.nref :> 0.nref
    !0 :> 0.nref
    (3.nref, 3.nref) :< (0.nref x 0.nref)
    (!0, !0) :< (0.nref x 0.nref)
    lam(3.nref) :< (0.nref ->: 0.nref)
    lam(!1) :< (0.nref ->: 0.nref)
    lam(3.nref) :< (1.nref ->: 0.nref)
    lam(!1) :< (1.nref ->: 0.nref)

    // 4 : 0 -> 1
    ctx.append(0.nref ->: 1.nref)

    4.nref(3.nref) :> 1.nref
    (!0)(!1) :> 1.nref

    // 5 : 0 x 1
    ctx.append(0.nref x 1.nref)

    p1(5.nref) :> 0.nref
    p1(!0) :> 0.nref
    p2(5.nref) :> 1.nref
    p2(!0) :> 1.nref

    lam(p1(!0)) :< ((0.nref x !0) ->: 0.nref)
    lam(p2(!0)) :< ((0.nref x !0) ->: p1(!0))
  }

  @Test
  def `more lambda type check` = {
    lam(!0) :< (set(0) ->: set(0))

    // standard id function
    lam(lam(!0)) :< (set(0) ->: !0 ->: !1)

    // (simple) compose function
    //            A   B   C   f   g   x  . f   (g   x)
    val compose = lam(lam(lam(lam(lam(lam((!2)((!1)(!0))))))))
    val composeTy = 
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
    compose :< composeTy
  }
}