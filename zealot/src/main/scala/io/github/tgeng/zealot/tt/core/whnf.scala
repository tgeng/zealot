package io.github.tgeng.zealot.tt.core

import scala.collection.immutable.Seq

class WhnfStuckException(val message: String, val errorContext: ErrorContext) extends Exception(message) {}

def (t: Term) whnf(given errCtx: ErrorContext): Whnf = t match {
  case Term.Ref(r) => Whnf.Neu(Neutral.Ref(r))
  case Term.Val(v) => Whnf.Val(v)
  case Term.Rdx(r) => r match {
    case Redux.App(fn, arg) => fn.whnf match {
      case Whnf.Neu(n) => Whnf.Neu(Neutral.Rdx(Redux.App(n, arg)))
      case Whnf.Val(v) => {
        v match {
          case Value.Lam(body) => body.substituteOutmost(arg).whnf
          case _ => throw WhnfStuckException(s"Expected $v to be a function.", errCtx)
        }
      }
    }
    case Redux.Prj1(p) => p.whnf match {
      case Whnf.Neu(n) => Whnf.Neu(Neutral.Rdx(Redux.Prj1(n)))
      case Whnf.Val(v) => v match {
        case Value.Pair(a, b) => a.whnf
        case _ => throw WhnfStuckException(s"Expected $v to be a pair.", errCtx)
      }
    }
    case Redux.Prj2(p) => p.whnf match {
      case Whnf.Neu(n) => Whnf.Neu(Neutral.Rdx(Redux.Prj2(n)))
      case Whnf.Val(v) => v match {
        case Value.Pair(a, b) => b.whnf
        case _ => throw WhnfStuckException("Expected " + v + " to be a pair.", errCtx)
      }
    }
  }
}

def (nf: Whnf) term = nf match {
  case Whnf.Neu(neu) => neutralToTerm(neu)
  case Whnf.Val(value) => Term.Val(value)
}
