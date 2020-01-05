package io.github.tgeng.zealot.tt.core

def context(types: Term*) : TypeContext = {
  val result = TypeContext()
  given errCtx: ErrorContext = Seq.empty
  for(t <- types) {
    result += t.whnf
  }
  result
}

def (ctx: TypeContext) +=(types: Term*) = {
  given errCtx: ErrorContext = Seq.empty
  for(t <- types) {
    ctx.+=(t.whnf)
  }
}
