package io.github.tgeng.zealot.tt.core

import scala.language.implicitConversions

object Builder {
  def t[S](t: Term[S]) = t

  def [S](index: Int) unary_! : Term[S] = Term.Ref(Reference.Idx(index)(None))(None)

  // Careful! You should not use this unless you are writing tests.
  def [S](index: Int) nref : Term[S] = Term.Ref(Reference.Num(index)(None))(None)

  def set[S](level: Int) = Term.Val(Value.Set(level)(None))(None)

  def [S](a: Term[S]) ->: (b: Term[S]) : Term[S] = Term.Val(Value.Pi(a, b)(None))(None)

  def lam[S](body: Term[S]) : Term[S] = Term.Val(Value.Lam(body)(None))(None)

  def [S](a: Term[S]) apply (b: Term[S]) = Term.Rdx(Redux.App(a, b)(None))(None)

  def [S](a: Term[S]) x (b: Term[S]) = Term.Val(Value.Sig(a, b)(None))(None)

  given tupleToPairConditional[A, B, S](given ac: A => Term[S])(given bc: B => Term[S]) : Conversion[(A, B), Term[S]] = (a, b) => Term.Val(Value.Pair(ac(a), bc(b))(None))(None)
  given tupleToPair[S] : Conversion[(Term[S], Term[S]), Term[S]] = (a, b) => Term.Val(Value.Pair(a, b)(None))(None)

  def p1[S](a: Term[S]) : Term[S] = Term.Rdx(Redux.Prj1(a)(None))(None)

  def p2[S](a: Term[S]) : Term[S] = Term.Rdx(Redux.Prj2(a)(None))(None)

  def unit[S] = Term.Val(Value.Unit()(None))

  def *[S] = Term.Val(Value.Star()(None))
}
