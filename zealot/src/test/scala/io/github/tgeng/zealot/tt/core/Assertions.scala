package io.github.tgeng.zealot.tt.core

import scala.collection.immutable.Seq
import io.github.tgeng.zealot.tt.core.TypeCheckOps._
import io.github.tgeng.zealot.tt.core.ErrorContext
import io.github.tgeng.fluentassert._

def haveWhnf(target: Term) = AssertionBase[Term]{ (t, objective) =>
  given errCtx : ErrorContext = Seq.empty
  val whnf = t.whnf.term
  if (objective && whnf != target) {
    Some(s"to have weak head normal form\n  $target\nbut it has\n  $whnf")
  } else if (!objective && whnf == target) {
    Some(s"to not have weak head normal form\n  $target")
  } else {
    None
  }
}

def checkWithType(ty: Term)(given ctx: TypeContext) = AssertionBase[Term] { (t, objective) =>
  (t.checkType(ty), objective) match {
    case (Left(e), true) => Some(s"to check with type\n  $ty\nbut it failed with message:\n${e.messageWithStackTrace(2)}")
    case (Right(_), false) => Some(s"to not check with type\n $ty")
    case _ => None
  }
}

def haveInferredType(ty: Term)(given ctx: TypeContext) = AssertionBase[Term] { (t, objective) =>
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

def (e: TypeCheckError) messageWithStackTrace(indent: Int) = {
  val indentString = " " * indent
  List(indentString + e.getMessage)
    .appended(indentString + "context:")
    .concat(e.ctx.zipWithIndex.map{ (t, i) =>
      s"$indentString  $i: $t"
    })
    .concat(
      e.errorContext.reverseIterator.map { op =>
        op match {
          case Check(t, ty) => s"checking $t against type $ty"
          case Infer(t) => s"inferring type of $t"
          case Subtype(a, b) => s"checking $a <= $b"
          case TermConvertible(a, b, ty) => s"checking $a ~= $b : $ty"
          case NeutralConvertible(a, b) => s"checking $a === $b"
        }
      }.map(indentString + "when " + _)
    ).mkString("\n")
}
