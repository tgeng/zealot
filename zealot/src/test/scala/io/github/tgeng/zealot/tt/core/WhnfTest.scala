package io.github.tgeng.zealot.tt.core

import scala.language.implicitConversions
import org.junit.Test
import io.github.tgeng.fluentassert._
import io.github.tgeng.zealot.tt.core._
import io.github.tgeng.zealot.tt.core.Builder.{given, _}

class WhnfTest {
  @Test
  def `value should remain as value` = {
    * ~~> *
    unit ~~> unit
    set(1) ~~> set(1)
    (!0 ->: !1) ~~> (!0 ->: !1)
    lam(!4) ~~> lam(!4)
    (!0 x !3) ~~> (!0 x !3)
    (!1, !2) ~~> (!1, !2)
  }

  @Test
  def `apply lambda` = {
    lam(!0)(*) ~~> *
    lam(!0)(!100) ~~> !100
    lam(lam((!1 , !0)))(!100)(!200) ~~> (!100, !200)
    lam(lam((!0 , !1)))(!100)(!200) ~~> (!200, !100)
    lam(!100)(!0) ~~> !99
    lam(!0 ->: !1)(!100) ~~> (!100 ->: !101)
    lam(!0 ->: !0)(!100) ~~> (!100 ->: !0)
    lam(!0 x !1)(!100) ~~> (!100 x !101)
    lam(!0 x !0)(!100) ~~> (!100 x !0)
  }

  @Test
  def `pair projection` = {
    p1((!1, !2)) ~~> !1
    p2((!1, !2)) ~~> !2
    p1((lam(!100), !4 x !5)) ~~> lam(!100)
    p2((lam(!100), !4 x !5)) ~~> (!4 x !5)
  }

  @Test
  def `does not reduce under head` = {
    (p1((!1, !2)) ->: !4) ~~> (p1((!1, !2)) ->: !4)
    (p1((!1, !2)) x !4) ~~> (p1((!1, !2)) x !4)
    lam(p1((!1, !2))) ~~> lam(p1((!1, !2)))
    t((p1((!1, !2)), !1)) ~~> t((p1((!1, !2)), !1))
    (!100)(p1((!1, !2))) ~~> (!100)(p1((!1, !2)))
  }

  @Test
  def `more convoluted cases` = {
    val apply = lam(lam((!1)(!0)))
    val extractFirst = lam(p1(!0))
    val pair = t(!100, !200)
    apply(extractFirst)(pair) ~~> !100
  }
}
