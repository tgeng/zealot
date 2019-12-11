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