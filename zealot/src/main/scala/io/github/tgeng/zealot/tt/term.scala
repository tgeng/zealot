package io.github.tgeng.zealot.tt

enum Term {
  case Ref(ref: Reference) extends Term
  case Val(value: Value) extends Term
  case Rdx(rdx: Redux[Term]) extends Term
}

enum Whnf {
  case Neu(neu: Neutral) extends Whnf
  case Val(value: Value) extends Whnf
}

enum Neutral {
  case Ref(ref: Reference) extends Neutral
  case Rdx(rdx: Redux[Neutral]) extends Neutral
}

enum Reference {
  case Idx(idx: Int)
  // Only used internally for type checking.
  case Num(num: Int)
}

enum Value {
  case Set(level: Int) extends Value
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

def (t: Term) substituteOutmost(substitute: Term) = t.substitute(0, substitute).raise(-1, 0)

def (t: Term) substitute(targetIndex: Int, substitute: Term) = 
  t.substituted(given SubstituteSpec(targetIndex: Int, substitute: Term))

private def (t: Term) raised(given spec: RaiseSpec) : Term = t match {
  case Term.Ref(Reference.Idx(i)) => if (i >= spec.bar) Term.Ref(Reference.Idx(i + spec.amount)) else t
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
  case Value.Pi(dom, cod) => Value.Pi(dom.raised, cod.raised(given spec++))
  case Value.Lam(body) => Value.Lam(body.raised(given spec++))
  case Value.Sig(fstTy, sndTy) => Value.Sig(fstTy.raised, sndTy.raised(given spec++))
  case Value.Pair(fst: Term, snd: Term) => Value.Pair(fst.raised, snd.raised)
  case _ => v
}

private def [T](r: Redux[T]) raised(tRaiser: T => (given RaiseSpec) => T)(given spec: RaiseSpec) : Redux[T] = r match {
  case Redux.App(fn, arg) => Redux.App(tRaiser(fn), arg.raised)
  case Redux.Prj1(t) => Redux.Prj1(tRaiser(t))
  case Redux.Prj2(t) => Redux.Prj2(tRaiser(t))
}

private def (t: Term) substituted(given spec: SubstituteSpec) : Term = t match {
  case Term.Ref(Reference.Idx(i)) => 
    if (i == spec.targetIndex) 
      spec.substitute
    else t
  case Term.Ref(_) => t
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
