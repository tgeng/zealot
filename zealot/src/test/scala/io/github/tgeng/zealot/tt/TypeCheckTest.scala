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