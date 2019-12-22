package io.github.tgeng.zealot.tt

import io.github.tgeng.fluentassert._

def haveWhnf(target: Term) = {
  Assertion[Term](
    t => {
      val whnf = t.whnf.term
      (s"normalize to\n  ${target}\nbut it normalizes to\n  $whnf", whnf == target)
    },
    true)
}

def (t1: Term) ~~> (t2: Term) = t1 should haveWhnf(t2)