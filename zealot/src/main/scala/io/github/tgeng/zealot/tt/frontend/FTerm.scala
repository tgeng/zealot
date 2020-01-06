package io.github.tgeng.zealot.tt.frontend

enum FTerm {
  case FRef(ref: FReference) extends FTerm
  case FVal(value: FValue) extends FTerm
  case FRdx(rdx: FRedux) extends FTerm
}

enum FReference {
  case FName(name: String)
}

enum FValue {
  case FSet(level: Int)
  case FPi(name: String, dom: FTerm, cod: FTerm)
  case FLam(name: String, body: FTerm)
  case FSig(name: String, fstTy: FTerm, sndTy: FTerm)
  case FPair(fst: FTerm, snd: FTerm)
  case FUnit()
  case FStar()
}

enum FRedux {
  case FApp(fn: FTerm, arg: FTerm)
  case FPrj1(pair: FTerm)
  case FPrj2(pair: FTerm)
}
