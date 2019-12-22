package io.github.tgeng.zealot.tt

class EvalStuckException(message: String) extends Exception(message) {}

def (t: Term) eval : NormalForm = t match {
  case Term.Var(i) => NormalForm.Neu(Neutral.Var(i))
  case Term.Val(v) => NormalForm.Val(v)
  case Term.Rdx(r) => r match {
    case Redux.App(fn, arg) => fn.eval match {
      case NormalForm.Neu(n) => NormalForm.Neu(Neutral.Rdx(Redux.App(n, arg)))
      case NormalForm.Val(v) => {
        val normalArg = arg.eval
        v match {
          case Value.Lam(body) => body.substitute(0, normalArg).raise(-1, 0).eval
          case _ => throw EvalStuckException("Expected " + v + " to be a function.")
        }
      }
    }
    case Redux.Prj1(p) => p.eval match {
      case NormalForm.Neu(n) => NormalForm.Neu(Neutral.Rdx(Redux.Prj1(n)))
      case NormalForm.Val(v) => v match {
        case Value.Pair(a, b) => a.eval
        case _ => throw EvalStuckException("Expected " + v + " to be a pair.")
      }
    }
    case Redux.Prj2(p) => p.eval match {
      case NormalForm.Neu(n) => NormalForm.Neu(Neutral.Rdx(Redux.Prj2(n)))
      case NormalForm.Val(v) => v match {
        case Value.Pair(a, b) => b.eval
        case _ => throw EvalStuckException("Expected " + v + " to be a pair.")
      }
    }
  }
}
