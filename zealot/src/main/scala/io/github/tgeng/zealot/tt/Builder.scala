package io.github.tgeng.zealot.tt

object Builder {
  def t(t: Term) = t

  def (index: Int) unary_! : Term = Term.Ref(Reference.Idx(index))

  def (index: Int) nref : Term = Term.Ref(Reference.Num(index))

  def set(id: Int) = Term.Val(Value.Set(id))

  def (a: Term) ->: (b: Term) : Term = Term.Val(Value.Pi(a, b))

  def lam(body: Term) : Term = Term.Val(Value.Lam(body))

  def (a: Term) apply (b: Term) = Term.Rdx(Redux.App(a, b))

  def (a: Term) x (b: Term) = Term.Val(Value.Sig(a, b))

  given tupleToPairConditional[A, B](given ac: A => Term)(given bc: B => Term) : Conversion[(A, B), Term] = (a, b) => Term.Val(Value.Pair(ac(a), bc(b)))

  given tupleToPair : Conversion[(Term, Term), Term] = (a, b) => Term.Val(Value.Pair(a, b))

  def p1(a: Term) = Term.Rdx(Redux.Prj1(a))

  def p2(a: Term) = Term.Rdx(Redux.Prj2(a))

  def unit = Term.Val(Value.Unit)

  def * = Term.Val(Value.Star)
}
