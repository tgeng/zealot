package io.github.tgeng.zealot.tt.core

def context(types: Term*)(given glbCtx: GlobalContext) : TypeContext = {
  val result = TypeContext()
  given errCtx: ErrorContext = Seq.empty
  for(t <- types) {
    result += t.whnf
  }
  result
}

def (ctx: TypeContext) +=(types: Term*)(given glbCtx: GlobalContext) = {
  given errCtx: ErrorContext = Seq.empty
  for(t <- types) {
    ctx.+=(t.whnf)
  }
}
