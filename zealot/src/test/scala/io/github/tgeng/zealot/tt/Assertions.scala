package io.github.tgeng.zealot.tt

import io.github.tgeng.fluentassert._

def haveWhnf(target: Term) = Assertion[Term]{ (t, objective) =>
  val whnf = t.whnf.term
  if (objective && whnf != target) {
    Some(s"to have weak head normal form\n  $target\nbut it has\n  $whnf")
  } else if (!objective && whnf == target) {
    Some(s"to not have weak head normal form\n  $target")
  } else {
    None
  }
}

def checkWithType(ty: Term)(given ctx: Context) = Assertion[Term] { (t, objective) =>
  (t.checkType(ty), objective) match {
    case (Left(e), true) => Some(s"to check with type\n  $ty\nbut it failed with message:\n${e.messageWithStackTrace(2)}")
    case (Right(_), false) => Some(s"to not check with type\n $ty")
    case _ => None
  }
}

def haveInferredType(ty: Term)(given ctx: Context) = Assertion[Term] { (t, objective) =>
  (t.inferType(), objective) match {
    case (Left(e), true) => Some(s"to have inferred type\n  $ty\nbut it failed with message:\n${e.messageWithStackTrace(2)}")
    case (Left(e), false) => Some(s"to not have inferred type\n  $ty\nbut it failed with message:\n${e.messageWithStackTrace(2)}")
    case (Right(inferredType), _) => 
    if (objective && inferredType.term != ty) {
      Some(s"to have inferred type\n  $ty\nbut it has inferred type\n  $inferredType")
    } else if (!objective && inferredType.term == ty) {
      Some(s"to not have inferred type\n  $ty")
    } else {
      None
    }
  }
}

def (t1: Term) ~~> (t2: Term) = t1 should haveWhnf(t2)
def (t1: Term) :< (t2: Term)(given ctx: Context) = t1 should checkWithType(t2)
def (t1: Term) :> (t2: Term)(given ctx: Context) = t1 should haveInferredType(t2)

def (e: Exception) messageWithStackTrace(indent: Int) = {
  val indentString = " " * indent
  e.getMessage + "\n" + e.getStackTrace.map{ indentString + _ }.mkString("\n")
}