package io.github.tgeng.zealot.tt

class EvalStuckException(message: String) extends Exception(message) {}

def (t: Term) whnf : Whnf = t match {
  case Term.Var(i) => Whnf.Neu(Neutral.Var(i))
  case Term.Val(v) => Whnf.Val(v)
  case Term.Rdx(r) => r match {
    case Redux.App(fn, arg) => fn.whnf match {
      case Whnf.Neu(n) => Whnf.Neu(Neutral.Rdx(Redux.App(n, arg)))
      case Whnf.Val(v) => {
        // TODO(tgeng): figure out how to do it better while keeping it clean.
        val normalArg = arg.whnf.term
        v match {
          case Value.Lam(body) => body.substitute(0, normalArg.raise(1, 0)).raise(-1, 0).whnf
          case _ => throw EvalStuckException(s"Expected $v to be a function.")
        }
      }
    }
    case Redux.Prj1(p) => p.whnf match {
      case Whnf.Neu(n) => Whnf.Neu(Neutral.Rdx(Redux.Prj1(n)))
      case Whnf.Val(v) => v match {
        case Value.Pair(a, b) => a.whnf
        case _ => throw EvalStuckException(s"Expected $v to be a pair.")
      }
    }
    case Redux.Prj2(p) => p.whnf match {
      case Whnf.Neu(n) => Whnf.Neu(Neutral.Rdx(Redux.Prj2(n)))
      case Whnf.Val(v) => v match {
        case Value.Pair(a, b) => b.whnf
        case _ => throw EvalStuckException("Expected " + v + " to be a pair.")
      }
    }
  }
}

def (nf: Whnf) term = nf match {
  case Whnf.Neu(neu) => neutralToTerm(neu)
  case Whnf.Val(value) => Term.Val(value)
}
