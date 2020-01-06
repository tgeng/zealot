package io.github.tgeng.zealot.tt.core

import scala.language.implicitConversions

object Builder {
  def t(t: Term) = t

  def (index: Int) unary_! : Term = Term.Ref(Reference.Idx(index))

  // Careful! You should not use this unless you are writing tests.
  def (index: Int) nref : Term = Term.Ref(Reference.Num(index))

  def set(level: Int) = Term.Val(Value.Set(level))

  def (a:(String, Term)) ->: (b: Term) : Term = Term.Val(Value.Pi(a._2, b)(Binder(a._1)))

  def lam(body: Term) : Term = lam("", body)
  def lam(name: String, body: Term) : Term = Term.Val(Value.Lam(body)(Binder(name)))

  def (a: Term) apply (b: Term) = Term.Rdx(Redux.App(a, b))

  def (a: (String, Term)) x (b: Term) = Term.Val(Value.Sig(a._2, b)(Binder(a._1)))

  given tupleToPairConditional[A, B](given ac: A => Term)(given bc: B => Term) : Conversion[(A, B), Term] = (a, b) => Term.Val(Value.Pair(ac(a), bc(b)))
  given tupleToPair : Conversion[(Term, Term), Term] = (a, b) => Term.Val(Value.Pair(a, b))

  def p1(a: Term) = Term.Rdx(Redux.Prj1(a))

  def p2(a: Term) = Term.Rdx(Redux.Prj2(a))

  def unit = Term.Val(Value.Unit)

  def * = Term.Val(Value.Star)

  given unnamedArg : Conversion[Term, (String, Term)] = ft => ("", ft)
}
