package io.github.tgeng.zealot.tt.frontend

import io.github.tgeng.zealot.tt.core.QualifiedName
import io.github.tgeng.zealot.tt.frontend.prettyPrint

enum FTerm {
  case FRef(ref: FReference) extends FTerm
  case FVal(value: FValue) extends FTerm
  case FRdx(rdx: FRedux) extends FTerm

  override def toString: String = {
    toString(ToStringSpec())
  }

  def toString(spec: ToStringSpec) : String = {
    val ctx = ToStringContext(spec)
    this.prettyPrint(ctx)
    ctx.getString
  }
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
  case FGlobal(qn: QualifiedName)
}
