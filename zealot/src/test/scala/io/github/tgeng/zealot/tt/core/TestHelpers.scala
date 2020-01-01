package io.github.tgeng.zealot.tt.core

def context(types: Term*) : Context = {
  val result = Context()
  given errCtx: ErrorContext = Seq.empty
  for(t <- types) {
    result.append(t.whnf)
  }
  result
}

def (ctx: Context) append(types: Term*) = {
  given errCtx: ErrorContext = Seq.empty
  for(t <- types) {
    ctx.append(t.whnf)
  }
}
