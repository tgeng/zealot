package io.github.tgeng.zealot.tt.core

import scala.collection.mutable.Buffer

enum Term {
  case Ref(ref: Reference)
  case Val(value: Value)
  case Rdx(rdx: Redux[Term])
}

enum Whnf {
  case Neu(neu: Neutral)
  case Val(value: Value)
}

enum Neutral {
  case Ref(ref: Reference)
  case Rdx(rdx: Redux[Neutral])
}

enum Reference {
  // De Bruijn index
  case Idx(idx: Int)
  // De Bruijn number (aka context size - 1 - index). Used by type checking
  // logic. Client code should not use this.
  case Num(num: Int)
}

// Additional information used during Term -> FTerm conversion. These data does not affect
// term evaluation and type checking.
class Binder(var name: String) {
  // All the other binders whose name could interfere with the name of this binder. Semantically
  // this should be a Set respecting object identity. But since the equals and hashcode method of
  // Value does not respect object identity we have to use a Seq here.
  val interferers: Buffer[Binder] = Buffer()

  def link(other: Binder) = {
    this.interferers += other
    other.interferers += this
  }
}

trait HasBinder {
  val binder: Binder
}

enum Value {
  case Set(level: Int)
  case Pi(dom: Term, cod: Term)(val binder: Binder) extends Value with HasBinder
  case Lam(body: Term)(val binder: Binder) extends Value with HasBinder
  case Sig(fstTy: Term, sndTy: Term)(val binder: Binder) extends Value with HasBinder
  case Pair(fst: Term, snd: Term)
  case Unit
  case Star
}

enum Redux[T] {
  case App(fn: T, arg: Term)
  case Prj1(pair: T)
  case Prj2(pair: T)
}

private def neutralToTerm(n: Neutral) : Term = n match {
  case Neutral.Ref(ref) => Term.Ref(ref)
  case Neutral.Rdx(rdx) => Term.Rdx(map(neutralToTerm)(rdx))
}

private def map[F, T](mapper: F => T)(input: Redux[F]) : Redux[T] = input match {
  case Redux.App(fn, arg) => Redux.App(mapper(fn), arg)
  case Redux.Prj1(pair) => Redux.Prj1(mapper(pair))
  case Redux.Prj2(pair) => Redux.Prj2(mapper(pair))
}

def (t: Term) raise(amount: Int, bar: Int) =
  t.raised(given RaiseSpec(amount, bar))

// def (t: Whnf) raise(amount: Int, bar: Int) =
//   t.raised(given RaiseSpec(amount, bar))

def (t: Term) substituteOutmost(substitute: Term) = t.substitute(0, substitute.raise(1, 0)).raise(-1, 0)


def (t: Term) substitute(targetIndex: Int, substitute: Term) =
  t.substituted(given SubstituteSpec(targetIndex: Int, substitute: Term))

private def (t: Term) raised(given spec: RaiseSpec) : Term = t match {
  case Term.Ref(Reference.Idx(i)) if (i >= spec.bar) => {
    assert (i + spec.amount >= 0)
    Term.Ref(Reference.Idx(i + spec.amount))
  }
  case Term.Ref(_) => t
  case Term.Val(v) => Term.Val(v.raised)
  case Term.Rdx(r) => Term.Rdx(r.raised{ _.raised })
}

// private def (nf: Whnf) raised(given spec: RaiseSpec): Whnf = nf match {
//   case Whnf.Neu(n) => Whnf.Neu(n.raised)
//   case Whnf.Val(v) => Whnf.Val(v.raised)
// }

// private def (n: Neutral) raised(given spec: RaiseSpec): Neutral = n match {
//   case Neutral.Var(i) => if (i >= spec.bar) Neutral.Var(i+1) else n
//   case Neutral.Rdx(r) => Neutral.Rdx(r.raised { _.raised })
// }

private def (v: Value) raised(given spec: RaiseSpec) : Value = v match {
  case v@Value.Pi(dom, cod) => Value.Pi(dom.raised, cod.raised(given spec++))(v.binder)
  case v@Value.Lam(body) => Value.Lam(body.raised(given spec++))(v.binder)
  case v@Value.Sig(fstTy, sndTy) => Value.Sig(fstTy.raised, sndTy.raised(given spec++))(v.binder)
  case Value.Pair(fst: Term, snd: Term) => Value.Pair(fst.raised, snd.raised)
  case _ => v
}

private def [T](r: Redux[T]) raised(tRaiser: T => (given RaiseSpec) => T)(given spec: RaiseSpec) : Redux[T] = r match {
  case Redux.App(fn, arg) => Redux.App(tRaiser(fn), arg.raised)
  case Redux.Prj1(t) => Redux.Prj1(tRaiser(t))
  case Redux.Prj2(t) => Redux.Prj2(tRaiser(t))
}

private def (t: Term) substituted(given spec: SubstituteSpec) : Term = t match {
  case Term.Ref(Reference.Idx(i)) if (i == spec.targetIndex) => spec.substitute
  case Term.Ref(_) => t
  case Term.Val(v) => Term.Val(v.substituted)
  case Term.Rdx(r) => Term.Rdx(r.substituted)
}

private def (v: Value) substituted(given spec: SubstituteSpec) : Value = v match {
  case v@Value.Pi(dom, cod) => Value.Pi(dom.substituted, cod.substituted(given spec++))(v.binder)
  case v@Value.Lam(body) => Value.Lam(body.substituted(given spec++))(v.binder)
  case v@Value.Sig(fstTy, sndTy) => Value.Sig(fstTy.substituted, sndTy.substituted(given spec++))(v.binder)
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
