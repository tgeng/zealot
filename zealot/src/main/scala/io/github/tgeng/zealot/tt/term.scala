package io.github.tgeng.zealot.tt

enum Term {
  case Var(idx: Int) extends Term
  case Universe(id: Int) extends Term
  case Pi(dom: Term, cod: Term) extends Term
  case Lam(body: Term) extends Term
  case App(fn: Term, arg: Term) extends Term
  case Sig(fstTy: Term, sndTy: Term) extends Term
  case Pair(fst: Term, snd: Term) extends Term
  case Prj1(pair: Term) extends Term
  case Prj2(pair: Term) extends Term
  case Unit extends Term
  case Star extends Term
}

def (t: Term) raise(amount: Int = 1, bar: Int = 0) = 
  t.raised(given RaiseSpec(amount, bar))

private case class RaiseSpec(amount: Int, bar: Int) {
  def ++ = RaiseSpec(amount, bar + 1)
}

private def (t: Term) raised(given spec: RaiseSpec) : Term = t match {
  case Term.Var(i) => if (i >= spec.bar) Term.Var(i + spec.amount) else t
  case Term.Universe(_) => t
  case Term.Pi(a, b) => Term.Pi(a.raised, b.raised(given spec++))
  case Term.Lam(b) => Term.Lam(b.raised(given spec++))
  case Term.App(a, b) => Term.App(a.raised, b.raised)
  case Term.Sig(fstTy, sndTy) => Term.Sig(fstTy.raised, sndTy.raised(given spec++))
  case Term.Pair(fst, snd) => Term.Pair(fst.raised, snd.raised)
  case Term.Prj1(pair) => Term.Prj1(pair.raised)
  case Term.Prj2(pair) => Term.Prj2(pair.raised)
  case Term.Unit => Term.Unit
  case Term.Star => Term.Star
}

def (t: Term) substitute(targetIndex: Int, substitute: Term) = 
  t.substituted(given SubstituteSpec(targetIndex: Int, substitute: Term))

private case class SubstituteSpec(targetIndex: Int, substitute: Term) {
  def ++ = SubstituteSpec(targetIndex + 1, substitute.raised(given RaiseSpec(1, 0)))
}

private def (t: Term) substituted(given spec: SubstituteSpec) : Term = t match {
  case Term.Var(i) => if (i == spec.targetIndex) spec.substitute else t
  case Term.Universe(_) => t
  case Term.Pi(a, b) => Term.Pi(a.substituted, b.substituted(given spec++))
  case Term.Lam(b) => Term.Lam(b.substituted(given spec++))
  case Term.App(a, b) => Term.App(a.substituted, b.substituted)
  case Term.Sig(fstTy, sndTy) => Term.Sig(fstTy.substituted, sndTy.substituted(given spec++))
  case Term.Pair(fst, snd) => Term.Pair(fst.substituted, snd.substituted)
  case Term.Prj1(pair) => Term.Prj1(pair.substituted)
  case Term.Prj2(pair) => Term.Prj2(pair.substituted)
  case Term.Unit => Term.Unit
  case Term.Star => Term.Star
}