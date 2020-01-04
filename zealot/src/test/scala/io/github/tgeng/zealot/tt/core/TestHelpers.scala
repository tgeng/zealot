package io.github.tgeng.zealot.tt.core

def context(types: Term*) : Context = {
  val result = Context()
  given errCtx: ErrorContext = Seq.empty
  for(t <- types) {
    result += t.whnf
  }
  result
}

def (ctx: Context) +=(types: Term*) = {
  given errCtx: ErrorContext = Seq.empty
  for(t <- types) {
    ctx.+=(t.whnf)
  }
}
