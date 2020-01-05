package io.github.tgeng.zealot.tt.core

import scala.collection.immutable.Seq

class WhnfStuckException[+S](val message: String, val errorContext: ErrorContext[S]) extends Exception(message) {}

def [S](t: Term[S]) whnf(given errCtx: ErrorContext[S]): Whnf[S] = t match {
  case Term.Ref(r) => Whnf.Neu(Neutral.Ref(r)(r.source))(t.source)
  case Term.Val(v) => Whnf.Val(v)(t.source)
  case Term.Rdx(r) => r match {
    case Redux.App(fn, arg) => fn.whnf[S] match {
      case Whnf.Neu(n) => Whnf.Neu(Neutral.Rdx(Redux.App(n, arg)(r.source))(t.source))(t.source)
      case Whnf.Val(v) => {
        v match {
          case Value.Lam(body) => body.substituteOutmost(arg).whnf
          case _ => throw WhnfStuckException(s"Expected $v to be a function.", errCtx)
        }
      }
    }
    case Redux.Prj1(p) => p.whnf[S] match {
      case Whnf.Neu(n) => Whnf.Neu(Neutral.Rdx(Redux.Prj1(n)(r.source))(t.source))(t.source)
      case Whnf.Val(v) => v match {
        case Value.Pair(a, b) => a.whnf[S]
        case _ => throw WhnfStuckException(s"Expected $v to be a pair.", errCtx)
      }
    }
    case Redux.Prj2(p) => p.whnf[S] match {
      case Whnf.Neu(n) => Whnf.Neu(Neutral.Rdx(Redux.Prj2(n)(r.source))(t.source))(t.source)
      case Whnf.Val(v) => v match {
        case Value.Pair(a, b) => b.whnf[S]
        case _ => throw WhnfStuckException("Expected " + v + " to be a pair.", errCtx)
      }
    }
  }
}

def [S](nf: Whnf[S]) term : Term[S] = nf match {
  case Whnf.Neu(neu) => neutralToTerm(neu)
  case Whnf.Val(value) => Term.Val(value)(nf.source)
}
