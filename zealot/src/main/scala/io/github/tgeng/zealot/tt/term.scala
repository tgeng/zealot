package io.github.tgeng.zealot.tt

enum Term {
  case Var(idx: Int) extends Term
  case Val(value: Value) extends Term
  case Rdx(rdx: Redux[Term]) extends Term
}

enum NormalForm {
  case Neu(neu: Neutral) extends NormalForm
  case Val(value: Value) extends NormalForm
}

enum Neutral {
  case Var(idx: Int) extends Neutral
  case Rdx(rdx: Redux[Neutral]) extends Neutral
}

enum Value {
  case Set(id: Int) extends Value
  case Pi(dom: Term, cod: Term) extends Value
  case Lam(body: Term) extends Value
  case Sig(fstTy: Term, sndTy: Term) extends Value
  case Pair(fst: Term, snd: Term) extends Value
  case Unit extends Value
  case Star extends Value
}

enum Redux[T] {
  case App(fn: T, arg: Term) extends Redux[T]
  case Prj1(pair: T) extends Redux[T]
  case Prj2(pair: T) extends Redux[T]
}

def (t: Term) raise(amount: Int = 1, bar: Int = 0) = 
  t.raised(given RaiseSpec(amount, bar))


def (t: Term) substitute(targetIndex: Int, substitute: Term) = 
  t.substituted(given SubstituteSpec(targetIndex: Int, substitute: Term))

private def (t: Term) raised(given spec: RaiseSpec) : Term = t match {
  case Term.Var(i) => if (i >= spec.bar) Term.Var(i + spec.amount) else t
  case Term.Val(v) => Term.Val(v.raised)
  case Term.Rdx(r) => Term.Rdx(r.raised)
}

private def (v: Value) raised(given spec: RaiseSpec) : Value = v match {
  case Value.Pi(dom, cod) => Value.Pi(dom.raised, cod.raised(given spec++))
  case Value.Lam(body) => Value.Lam(body.raised(given spec++))
  case Value.Sig(fstTy, sndTy) => Value.Sig(fstTy.raised, sndTy.raised(given spec++))
  case Value.Pair(fst: Term, snd: Term) => Value.Pair(fst.raised, snd.raised)
  case _ => v
}

private def (r: Redux[Term]) raised(given spec: RaiseSpec) : Redux[Term] = r match {
  case Redux.App(fn, arg) => Redux.App(fn.raised, arg.raised)
  case Redux.Prj1(t) => Redux.Prj1(t.raised)
  case Redux.Prj2(t) => Redux.Prj2(t.raised)
}

private def (t: Term) substituted(given spec: SubstituteSpec) : Term = t match {
  case Term.Var(i) => if (i == spec.targetIndex) spec.substitute else t
  case Term.Val(v) => Term.Val(v.substituted)
  case Term.Rdx(r) => Term.Rdx(r.substituted)
}

private def (v: Value) substituted(given spec: SubstituteSpec) : Value = v match {
  case Value.Pi(dom, cod) => Value.Pi(dom.substituted, cod.substituted(given spec++))
  case Value.Lam(body) => Value.Lam(body.substituted(given spec++))
  case Value.Sig(fstTy, sndTy) => Value.Sig(fstTy.substituted, sndTy.substituted(given spec++))
  case Value.Pair(fst: Term, snd: Term) => Value.Pair(fst.substituted, snd.substituted)
  case _ => v
}

private def (r: Redux[Term]) substituted(given spec: SubstituteSpec) : Redux[Term] = r match{
  case Redux.App(fn, arg) => Redux.App(fn.substituted, arg.substituted)
  case Redux.Prj1(t) => Redux.Prj1(t.substituted)
  case Redux.Prj2(t) => Redux.Prj2(t.substituted)
}

private case class RaiseSpec(amount: Int, bar: Int) {
  def ++ = RaiseSpec(amount, bar + 1)
}

private case class SubstituteSpec(targetIndex: Int, substitute: Term) {
  def ++ = SubstituteSpec(targetIndex + 1, substitute.raised(given RaiseSpec(1, 0)))
}