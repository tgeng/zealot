package io.github.tgeng.zealot.tt.core

import scala.language.implicitConversions
import org.junit.Test
import io.github.tgeng.fluentassert._
import io.github.tgeng.zealot.tt.core._
import io.github.tgeng.zealot.tt.core.Builder.{given, _}

class WhnfTest {

  given glbCtx: GlobalContext = GlobalContext()
  given typeCtx: TypeContext = TypeContext()

  @Test
  def `value should remain as value` = {
    star ~~> star
    unit ~~> unit
    set(1) ~~> set(1)
    (0.ref ->: 1.ref) ~~> (0.ref ->: 1.ref)
    lam(4.ref) ~~> lam(4.ref)
    (0.ref &: 3.ref) ~~> (0.ref &: 3.ref)
    (1.ref, 2.ref) ~~> (1.ref, 2.ref)
  }

  @Test
  def `apply lambda` = {
    lam(0.ref)(star) ~~> star
    lam(0.ref)(100.ref) ~~> 100.ref
    lam(lam((1.ref , 0.ref)))(100.ref)(200.ref) ~~> (100.ref, 200.ref)
    lam(lam((0.ref , 1.ref)))(100.ref)(200.ref) ~~> (200.ref, 100.ref)
    lam(100.ref)(0.ref) ~~> 99.ref
    lam(0.ref ->: 1.ref)(100.ref) ~~> (100.ref ->: 101.ref)
    lam(0.ref ->: 0.ref)(100.ref) ~~> (100.ref ->: 0.ref)
    lam(0.ref &: 1.ref)(100.ref) ~~> (100.ref &: 101.ref)
    lam(0.ref &: 0.ref)(100.ref) ~~> (100.ref &: 0.ref)
  }

  @Test
  def `pair projection` = {
    p1((1.ref, 2.ref)) ~~> 1.ref
    p2((1.ref, 2.ref)) ~~> 2.ref
    p1((lam(100.ref), 4.ref &: 5.ref)) ~~> lam(100.ref)
    p2((lam(100.ref), 4.ref &: 5.ref)) ~~> (4.ref &: 5.ref)
  }

  @Test
  def `does not reduce under head` = {
    (p1((1.ref, 2.ref)) ->: 4.ref) ~~> (p1((1.ref, 2.ref)) ->: 4.ref)
    (p1((1.ref, 2.ref)) &: 4.ref) ~~> (p1((1.ref, 2.ref)) &: 4.ref)
    lam(p1((1.ref, 2.ref))) ~~> lam(p1((1.ref, 2.ref)))
    t((p1((1.ref, 2.ref)), 1.ref)) ~~> t((p1((1.ref, 2.ref)), 1.ref))
    (100.ref)(p1((1.ref, 2.ref))) ~~> (100.ref)(p1((1.ref, 2.ref)))
  }

  @Test
  def `more convoluted cases` = {
    val apply = lam(lam((1.ref)(0.ref)))
    val extractFirst = lam(p1(0.ref))
    val pair = t(100.ref, 200.ref)
    apply(extractFirst)(pair) ~~> 100.ref
  }

  @Test
  def `global reference` = {
    glbCtx(root/"abc") = (root/"def", set(0))
    glbCtx(root/"def") = (unit, set(0))

    t(root/"def") ~~> unit
  }

  @Test
  def `inductive type and value` = {
    val natName = root/"Nat"
    val nat = t(natName)
    val zero = t(natName/"Zero")
    val suc = t(natName/"Suc")
    val natSchema = natName |: set(0) where {
      "Zero" |: nat
      "Suc" |: nat ->: nat
    }

    zero ~~> vcon(natSchema("Zero"))
    suc(zero) ~~> vcon(natSchema("Suc"), zero)

    val finName = root/"Fin"
    val fin = t(finName)
    val fzero = t(finName/"Zero")
    val fsuc = t(finName/"Suc")
    val finSchema = finName |: nat ->: set(0) where {
      "Zero" |: nat ->: fin(suc(0.ref))
      "Suc" |: nat ->: fin(0.ref) ->: fin(suc(1.ref))
    }

    fzero(suc(zero)) ~~> vcon(finSchema("Zero"), suc(zero))
    fsuc(suc(zero))(fzero(suc(zero))) ~~> vcon(finSchema("Suc"), suc(zero), fzero(suc(zero)))
  }

  def (t1: Term) ~~> (t2: Term) = t1 should haveWhnf(t2)
}
